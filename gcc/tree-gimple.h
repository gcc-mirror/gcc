/* Functions to analyze and validate GIMPLE trees.
   Copyright (C) 2002, 2003, 2005 Free Software Foundation, Inc.
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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef _TREE_SIMPLE_H
#define _TREE_SIMPLE_H 1


#include "tree-iterator.h"

extern tree create_tmp_var_raw (tree, const char *);
extern tree create_tmp_var_name (const char *);
extern tree create_tmp_var (tree, const char *);
extern tree get_initialized_tmp_var (tree, tree *, tree *);
extern tree get_formal_tmp_var (tree, tree *);
extern void declare_tmp_vars (tree, tree);

extern void annotate_all_with_locus (tree *, location_t);

/* Validation of GIMPLE expressions.  Note that these predicates only check
   the basic form of the expression, they don't recurse to make sure that
   underlying nodes are also of the right form.  */

typedef bool (*gimple_predicate)(tree);

/* Returns true iff T is a valid GIMPLE statement.  */
extern bool is_gimple_stmt (tree);

/* Returns true iff TYPE is a valid type for a scalar register variable.  */
extern bool is_gimple_reg_type (tree);
/* Returns true iff T is a scalar register variable.  */
extern bool is_gimple_reg (tree);
/* Returns true if T is a GIMPLE temporary variable, false otherwise.  */
extern bool is_gimple_formal_tmp_var (tree);
/* Returns true if T is a GIMPLE temporary register variable.  */
extern bool is_gimple_formal_tmp_reg (tree);
/* Returns true iff T is any sort of variable.  */
extern bool is_gimple_variable (tree);
/* Returns true iff T is a variable or an INDIRECT_REF (of a variable).  */
extern bool is_gimple_min_lval (tree);
/* Returns true iff T is something whose address can be taken.  */
extern bool is_gimple_addressable (tree);
/* Returns true iff T is any valid GIMPLE lvalue.  */
extern bool is_gimple_lvalue (tree);

/* Returns true iff T is a GIMPLE restricted function invariant.  */
extern bool is_gimple_min_invariant (tree);
/* Returns true iff T is a GIMPLE rvalue.  */
extern bool is_gimple_val (tree);
/* Returns true iff T is a GIMPLE asm statement input.  */
extern bool is_gimple_asm_val (tree);
/* Returns true iff T is a valid rhs for a MODIFY_EXPR where the LHS is a
   GIMPLE temporary, a renamed user variable, or something else,
   respectively.  */
extern bool is_gimple_formal_tmp_rhs (tree);
extern bool is_gimple_reg_rhs (tree);
extern bool is_gimple_mem_rhs (tree);
/* Returns the appropriate one of the above three predicates for the LHS
   T.  */
extern gimple_predicate rhs_predicate_for (tree);

/* Returns true iff T is a valid if-statement condition.  */
extern bool is_gimple_condexpr (tree);

/* Returns true iff T is a type conversion.  */
extern bool is_gimple_cast (tree);
/* Returns true iff T is a variable that does not need to live in memory.  */
extern bool is_gimple_non_addressable (tree t);

/* Returns true iff T is a valid call address expression.  */
extern bool is_gimple_call_addr (tree);
/* If T makes a function call, returns the CALL_EXPR operand.  */
extern tree get_call_expr_in (tree t);

extern void recalculate_side_effects (tree);

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

extern enum gimplify_status gimplify_expr (tree *, tree *, tree *,
					   bool (*) (tree), fallback_t);
extern void gimplify_type_sizes (tree, tree *);
extern void gimplify_one_sizepos (tree *, tree *);
extern void gimplify_stmt (tree *);
extern void gimplify_to_stmt_list (tree *);
extern void gimplify_body (tree *, tree, bool);
extern void push_gimplify_context (void);
extern void pop_gimplify_context (tree);
extern void gimplify_and_add (tree, tree *);

/* Miscellaneous helpers.  */
extern void gimple_add_tmp_var (tree);
extern tree gimple_current_bind_expr (void);
extern tree voidify_wrapper_expr (tree, tree);
extern tree gimple_build_eh_filter (tree, tree, tree);
extern tree build_and_jump (tree *);
extern tree alloc_stmt_list (void);
extern void free_stmt_list (tree);
extern tree force_labels_r (tree *, int *, void *);
extern enum gimplify_status gimplify_va_arg_expr (tree *, tree *, tree *);

/* In tree-nested.c.  */
extern void lower_nested_functions (tree);

#endif /* _TREE_SIMPLE_H  */
