// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_AST_RESOLVE_STMT_H
#define RUST_AST_RESOLVE_STMT_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-full.h"
#include "rust-ast-resolve-type.h"
#include "rust-ast-resolve-pattern.h"
#include "rust-ast-resolve-expr.h"

namespace Rust {
namespace Resolver {

class ResolveStmt : public ResolverBase
{
public:
  static void go (AST::Stmt *stmt, NodeId parent)
  {
    ResolveStmt resolver (parent);
    stmt->accept_vis (resolver);
  };

  ~ResolveStmt () {}

  void visit (AST::ExprStmtWithBlock &stmt)
  {
    ResolveExpr::go (stmt.get_expr ().get (), stmt.get_node_id ());
  }

  void visit (AST::ExprStmtWithoutBlock &stmt)
  {
    ResolveExpr::go (stmt.get_expr ().get (), stmt.get_node_id ());
  }

  void visit (AST::LetStmt &stmt)
  {
    if (stmt.has_init_expr ())
      {
	ResolveExpr::go (stmt.get_init_expr ().get (), stmt.get_node_id ());

	// mark the assignment
	resolver->mark_assignment_to_decl (stmt.get_pattern ()->get_node_id (),
					   stmt.get_node_id ());
      }

    PatternDeclaration::go (stmt.get_pattern ().get (), stmt.get_node_id ());
    if (stmt.has_type ())
      ResolveType::go (stmt.get_type ().get (), stmt.get_node_id ());
  }

private:
  ResolveStmt (NodeId parent) : ResolverBase (parent) {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_STMT_H
