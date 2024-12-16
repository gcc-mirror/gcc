// Copyright (C) 2021-2025 Free Software Foundation, Inc.

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
#include "print-tree.h"

namespace Rust {
namespace Analysis {

static std::map<tree, int> assignment_map = {};

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
emit_error (tree *t, tree lhs, enum lvalue_use use)
{
  readonly_error (EXPR_LOCATION (*t), lhs, use);
  TREE_OPERAND (*t, 0) = error_mark_node;
}

static void
check_modify_expr (tree *t)
{
  tree lhs = TREE_OPERAND (*t, 0);
  if (TREE_CODE (lhs) == ARRAY_REF || TREE_CODE (lhs) == COMPONENT_REF)
    lhs = TREE_OPERAND (lhs, 0);

  tree lhs_type = TREE_TYPE (lhs);
  if (TYPE_READONLY (lhs_type) || TREE_READONLY (lhs) || TREE_CONSTANT (lhs))
    {
      if (TREE_CODE (lhs) != VAR_DECL)
	emit_error (t, lhs, lv_assign);
      else if (!DECL_ARTIFICIAL (lhs))
	{
	  if (DECL_INITIAL (lhs) != NULL)
	    emit_error (t, lhs, lv_assign);
	  else
	    {
	      if (assignment_map.find (lhs) == assignment_map.end ())
		{
		  assignment_map.insert ({lhs, 0});
		}
	      assignment_map[lhs]++;

	      if (assignment_map[lhs] > 1)
		emit_error (t, lhs, lv_assign);
	    }
	}
    }
}

static void
check_decl (tree *t)
{
  switch (TREE_CODE (*t))
    {
    case MODIFY_EXPR:
      check_modify_expr (t);
      break;

    default:
      break;
    }
}

static tree
readonly_walk_fn (tree *t, int *, void *)
{
  check_decl (t);
  return NULL_TREE;
}

void
ReadonlyCheck::Lint (Compile::Context &ctx)
{
  assignment_map.clear ();
  for (auto &fndecl : ctx.get_func_decls ())
    {
      for (tree p = DECL_ARGUMENTS (fndecl); p != NULL_TREE; p = DECL_CHAIN (p))
	{
	  check_decl (&p);
	}

      walk_tree_without_duplicates (&DECL_SAVED_TREE (fndecl),
				    &readonly_walk_fn, &ctx);
    }

  assignment_map.clear ();
  for (auto &var : ctx.get_var_decls ())
    {
      tree decl = var->get_decl ();
      check_decl (&decl);
    }

  assignment_map.clear ();
  for (auto &const_decl : ctx.get_const_decls ())
    {
      check_decl (&const_decl);
    }
}

} // namespace Analysis
} // namespace Rust
