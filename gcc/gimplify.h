/* Header file for gimplification.
   Copyright (C) 2013 Free Software Foundation, Inc.

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

#ifndef GCC_GIMPLIFY_H
#define GCC_GIMPLIFY_H

#include "gimple.h"

/* Validation of GIMPLE expressions.  Note that these predicates only check
   the basic form of the expression, they don't recurse to make sure that
   underlying nodes are also of the right form.  */
typedef bool (*gimple_predicate)(tree);

/* FIXME we should deduce this from the predicate.  */
enum fallback {
  fb_none = 0,		/* Do not generate a temporary.  */

  fb_rvalue = 1,	/* Generate an rvalue to hold the result of a
			   gimplified expression.  */

  fb_lvalue = 2,	/* Generate an lvalue to hold the result of a
			   gimplified expression.  */

  fb_mayfail = 4,	/* Gimplification may fail.  Error issued
			   afterwards.  */
  fb_either= fb_rvalue | fb_lvalue
};

typedef int fallback_t;

enum gimplify_status {
  GS_ERROR	= -2,	/* Something Bad Seen.  */
  GS_UNHANDLED	= -1,	/* A langhook result for "I dunno".  */
  GS_OK		= 0,	/* We did something, maybe more to do.  */
  GS_ALL_DONE	= 1	/* The expression is fully gimplified.  */
};

extern void push_gimplify_context (struct gimplify_ctx *);
extern void pop_gimplify_context (gimple);
extern gimple gimple_current_bind_expr (void);
extern vec<gimple> gimple_bind_expr_stack (void);
extern void gimplify_and_add (tree, gimple_seq *);
extern tree get_formal_tmp_var (tree, gimple_seq *);
extern tree get_initialized_tmp_var (tree, gimple_seq *, gimple_seq *);
extern void declare_vars (tree, gimple, bool);
extern void gimple_add_tmp_var (tree);
extern tree unshare_expr (tree);
extern tree unshare_expr_without_location (tree);
extern tree voidify_wrapper_expr (tree, tree);
extern tree build_and_jump (tree *);
extern enum gimplify_status gimplify_self_mod_expr (tree *, gimple_seq *,
						    gimple_seq *, bool, tree);
extern tree gimple_boolify (tree);
extern bool gimplify_stmt (tree *, gimple_seq *);
extern void omp_firstprivatize_variable (struct gimplify_omp_ctx *, tree);
extern enum gimplify_status gimplify_expr (tree *, gimple_seq *, gimple_seq *,
					   bool (*) (tree), fallback_t);

extern void gimplify_type_sizes (tree, gimple_seq *);
extern void gimplify_one_sizepos (tree *, gimple_seq *);
extern gimple gimplify_body (tree, bool);
extern void gimplify_function_tree (tree);
extern void gimple_regimplify_operands (gimple, gimple_stmt_iterator *);
extern tree force_gimple_operand_1 (tree, gimple_seq *, gimple_predicate, tree);
extern tree force_gimple_operand (tree, gimple_seq *, bool, tree);
extern tree force_gimple_operand_gsi_1 (gimple_stmt_iterator *, tree,
					gimple_predicate, tree,
					bool, enum gsi_iterator_update);
extern tree force_gimple_operand_gsi (gimple_stmt_iterator *, tree, bool, tree,
				      bool, enum gsi_iterator_update);

extern enum gimplify_status gimplify_va_arg_expr (tree *, gimple_seq *,
						  gimple_seq *);
gimple gimplify_assign (tree, tree, gimple_seq *);

#endif /* GCC_GIMPLIFY_H */
