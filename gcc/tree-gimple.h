/* Functions to analyze and validate GIMPLE trees.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef _TREE_SIMPLE_H
#define _TREE_SIMPLE_H 1


#include "tree-iterator.h"

extern tree create_tmp_var_raw (tree, const char *);
extern tree create_tmp_var (tree, const char *);
extern bool is_gimple_tmp_var (tree);
extern tree get_initialized_tmp_var (tree, tree *, tree *);
extern tree get_formal_tmp_var (tree, tree *);
extern void declare_tmp_vars (tree, tree);

extern tree rationalize_compound_expr (tree);
extern tree right_assocify_expr (tree);
extern void annotate_all_with_locus (tree *, location_t);

/* Validation of GIMPLE expressions.  Note that these predicates only check
   the basic form of the expression, they don't recurse to make sure that
   underlying nodes are also of the right form.  */

/* Returns true iff T is a valid GIMPLE statement.  */
bool is_gimple_stmt (tree);

/* Returns true iff TYPE is a valid type for a scalar register variable.  */
bool is_gimple_reg_type (tree);
/* Returns true iff T is a scalar register variable.  */
bool is_gimple_reg (tree);
/* Returns true iff T is any sort of variable.  */
bool is_gimple_variable (tree);
/* Returns true iff T is a variable or an INDIRECT_REF (of a variable).  */
bool is_gimple_min_lval (tree);
/* Returns true iff T is an lvalue other than an INDIRECT_REF.  */
bool is_gimple_addr_expr_arg (tree);
/* Returns true iff T is any valid GIMPLE lvalue.  */
bool is_gimple_lvalue (tree);

/* Returns true iff T is a GIMPLE restricted function invariant.  */
bool is_gimple_min_invariant (tree);
/* Returns true iff T is a GIMPLE rvalue.  */
bool is_gimple_val (tree);
/* Returns true iff T is a valid rhs for a MODIFY_EXPR.  */
bool is_gimple_rhs (tree);

/* Returns true iff T is a valid if-statement condition.  */
bool is_gimple_condexpr (tree);

/* Returns true iff T is a type conversion.  */
bool is_gimple_cast (tree);
/* Returns true iff T is a valid CONSTRUCTOR element (either an rvalue or
   another CONSTRUCTOR).  */
bool is_gimple_constructor_elt (tree);
/* Returns true iff T is a variable that does not need to live in memory.  */
bool is_gimple_non_addressable (tree t);

/* If T makes a function call, returns the CALL_EXPR operand.  */
tree get_call_expr_in (tree t);

void recalculate_side_effects (tree);

void append_to_statement_list (tree, tree *);
void append_to_statement_list_force (tree, tree *);
void append_to_compound_expr (tree, tree *);

/* FIXME we should deduce this from the predicate.  */
typedef enum fallback_t {
  fb_none = 0,
  fb_rvalue = 1,
  fb_lvalue = 2,
  fb_mayfail = 4,
  fb_either= fb_rvalue | fb_lvalue
} fallback_t;

enum gimplify_status {
  GS_ERROR	= -2,	/* Something Bad Seen.  */
  GS_UNHANDLED	= -1,	/* A langhook result for "I dunno".  */
  GS_OK		= 0,	/* We did something, maybe more to do.  */
  GS_ALL_DONE	= 1	/* The expression is fully gimplified.  */
};

enum gimplify_status gimplify_expr (tree *, tree *, tree *,
				    bool (*) (tree), fallback_t);
void gimplify_stmt (tree *);
void gimplify_to_stmt_list (tree *);
void gimplify_body (tree *, tree);
void push_gimplify_context (void);
void pop_gimplify_context (tree);

/* Miscellaneous helpers.  */
tree get_base_address (tree t);
void gimple_add_tmp_var (tree);
tree gimple_current_bind_expr (void);
void gimple_push_bind_expr (tree);
void gimple_pop_bind_expr (void);
void unshare_all_trees (tree);
tree voidify_wrapper_expr (tree);
tree gimple_build_eh_filter (tree, tree, tree);
tree build_and_jump (tree *);
tree alloc_stmt_list (void);
void free_stmt_list (tree);
tree force_labels_r (tree *, int *, void *);

/* In tree-nested.c.  */
extern void lower_nested_functions (tree);

#endif /* _TREE_SIMPLE_H  */
