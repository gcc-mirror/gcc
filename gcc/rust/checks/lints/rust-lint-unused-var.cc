// Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

#include "rust-lint-unused-var.h"
#include "print-tree.h"

namespace Rust {
namespace Analysis {

static void
check_decl (tree *t)
{
  rust_assert (TREE_CODE (*t) == VAR_DECL || TREE_CODE (*t) == PARM_DECL
	       || TREE_CODE (*t) == CONST_DECL);

  tree var_name = DECL_NAME (*t);
  const char *var_name_ptr = IDENTIFIER_POINTER (var_name);
  bool starts_with_under_score = strncmp (var_name_ptr, "_", 1) == 0;

  bool is_constant = TREE_CODE (*t) == CONST_DECL;
  // if (!is_constant)
  //   {
  //     debug_tree (*t);
  //     rust_debug ("found var-decl: used %s artifical %s underscore %s name
  //     %s",
  //       	  TREE_USED (*t) ? "true" : "false",
  //       	  DECL_ARTIFICIAL (*t) ? "true" : "false",
  //       	  starts_with_under_score ? "true" : "false", var_name_ptr);
  //   }

  if (!TREE_USED (*t) && !DECL_ARTIFICIAL (*t) && !starts_with_under_score)
    {
      warning_at (DECL_SOURCE_LOCATION (*t),
		  is_constant ? OPT_Wunused_const_variable_
			      : OPT_Wunused_variable,
		  "unused name %qE", *t);
    }
}

static tree
unused_var_walk_fn (tree *t, int *, void *)
{
  switch (TREE_CODE (*t))
    {
    case VAR_DECL:
    case CONST_DECL:
      check_decl (t);
      break;

    default:
      break;
    }
  return NULL_TREE;
}

void
UnusedVariables::Lint (Compile::Context &ctx)
{
  for (auto &fndecl : ctx.get_func_decls ())
    {
      for (tree p = DECL_ARGUMENTS (fndecl); p != NULL_TREE; p = DECL_CHAIN (p))
	{
	  check_decl (&p);
	}

      walk_tree_without_duplicates (&DECL_SAVED_TREE (fndecl),
				    &unused_var_walk_fn, &ctx);
    }

  for (auto &var : ctx.get_var_decls ())
    {
      tree t = ctx.get_backend ()->var_expression (var, Location ());
      check_decl (&t);
    }

  for (auto &const_decl : ctx.get_const_decls ())
    {
      check_decl (&const_decl);
    }
}

} // namespace Analysis
} // namespace Rust
