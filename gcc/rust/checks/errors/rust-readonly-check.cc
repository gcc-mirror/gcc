// Copyright (C) 2021-2024 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-readonly-check.h"
#include "rust-tree.h"
#include "rust-gcc.h"

namespace Rust {
namespace Analysis {

// ported over from c-family/c-warn.cc
void
readonly_error (location_t loc, tree arg, enum lvalue_use use)
{
  gcc_assert (use == lv_assign || use == lv_increment || use == lv_decrement
	      || use == lv_asm);
  STRIP_ANY_LOCATION_WRAPPER (arg);
  /* Using this macro rather than (for example) arrays of messages
     ensures that all the format strings are checked at compile
     time.  */
#define READONLY_MSG(A, I, D, AS)                                              \
  (use == lv_assign                                                            \
     ? (A)                                                                     \
     : (use == lv_increment ? (I) : (use == lv_decrement ? (D) : (AS))))
  if (TREE_CODE (arg) == COMPONENT_REF)
    {
      if (TYPE_READONLY (TREE_TYPE (TREE_OPERAND (arg, 0))))
	error_at (loc,
		  READONLY_MSG (G_ ("assignment of member "
				    "%qD in read-only object"),
				G_ ("increment of member "
				    "%qD in read-only object"),
				G_ ("decrement of member "
				    "%qD in read-only object"),
				G_ ("member %qD in read-only object "
				    "used as %<asm%> output")),
		  TREE_OPERAND (arg, 1));
      else
	error_at (
	  loc,
	  READONLY_MSG (G_ ("assignment of read-only member %qD"),
			G_ ("increment of read-only member %qD"),
			G_ ("decrement of read-only member %qD"),
			G_ ("read-only member %qD used as %<asm%> output")),
	  TREE_OPERAND (arg, 1));
    }
  else if (VAR_P (arg))
    error_at (loc,
	      READONLY_MSG (G_ ("assignment of read-only variable %qD"),
			    G_ ("increment of read-only variable %qD"),
			    G_ ("decrement of read-only variable %qD"),
			    G_ (
			      "read-only variable %qD used as %<asm%> output")),
	      arg);
  else if (TREE_CODE (arg) == PARM_DECL)
    error_at (loc,
	      READONLY_MSG (G_ ("assignment of read-only parameter %qD"),
			    G_ ("increment of read-only parameter %qD"),
			    G_ ("decrement of read-only parameter %qD"),
			    G_ (
			      "read-only parameter %qD use as %<asm%> output")),
	      arg);
  else if (TREE_CODE (arg) == RESULT_DECL)
    {
      error_at (loc,
		READONLY_MSG (G_ ("assignment of "
				  "read-only named return value %qD"),
			      G_ ("increment of "
				  "read-only named return value %qD"),
			      G_ ("decrement of "
				  "read-only named return value %qD"),
			      G_ ("read-only named return value %qD "
				  "used as %<asm%>output")),
		arg);
    }
  else if (TREE_CODE (arg) == FUNCTION_DECL)
    error_at (loc,
	      READONLY_MSG (G_ ("assignment of function %qD"),
			    G_ ("increment of function %qD"),
			    G_ ("decrement of function %qD"),
			    G_ ("function %qD used as %<asm%> output")),
	      arg);
  else
    error_at (loc,
	      READONLY_MSG (G_ ("assignment of read-only location %qE"),
			    G_ ("increment of read-only location %qE"),
			    G_ ("decrement of read-only location %qE"),
			    G_ (
			      "read-only location %qE used as %<asm%> output")),
	      arg);
}

static void
check_decl (tree *t)
{
  if (TREE_CODE (*t) == MODIFY_EXPR)
    {
      tree lhs = TREE_OPERAND (*t, 0);
      if (TREE_READONLY (lhs) || TREE_CONSTANT (lhs))
	{
	  readonly_error (EXPR_LOCATION (*t), lhs, lv_assign);
	  TREE_OPERAND (*t, 0) = error_mark_node;
	}
    }
}

static tree
readonly_walk_fn (tree *t, int *, void *)
{
  switch (TREE_CODE (*t))
    {
    case MODIFY_EXPR:
      check_decl (t);
      break;

    default:
      break;
    }
  return NULL_TREE;
}

void
ReadonlyCheck::Lint (Compile::Context &ctx)
{
  for (auto &fndecl : ctx.get_func_decls ())
    {
      for (tree p = DECL_ARGUMENTS (fndecl); p != NULL_TREE; p = DECL_CHAIN (p))
	{
	  check_decl (&p);
	}

      walk_tree_without_duplicates (&DECL_SAVED_TREE (fndecl),
				    &readonly_walk_fn, &ctx);
    }

  for (auto &var : ctx.get_var_decls ())
    {
      tree decl = var->get_decl ();
      check_decl (&decl);
    }

  for (auto &const_decl : ctx.get_const_decls ())
    {
      check_decl (&const_decl);
    }
}

} // namespace Analysis
} // namespace Rust
