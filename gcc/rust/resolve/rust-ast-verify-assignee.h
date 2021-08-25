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

#ifndef RUST_AST_VERIFY_ASSIGNEE
#define RUST_AST_VERIFY_ASSIGNEE

#include "rust-ast-resolve-base.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Resolver {

class VerifyAsignee : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static bool go (AST::Expr *assignee, NodeId parent)
  {
    VerifyAsignee checker (parent);
    assignee->accept_vis (checker);
    if (!checker.ok)
      rust_error_at (assignee->get_locus (),
		     "invalid left-hand side of assignment");
    return checker.ok;
  }

  void visit (AST::ArrayIndexExpr &expr) override
  {
    expr.get_array_expr ()->accept_vis (*this);
  }

  void visit (AST::FieldAccessExpr &expr) override
  {
    expr.get_receiver_expr ()->accept_vis (*this);
  }

  void visit (AST::TupleIndexExpr &expr) override
  {
    expr.get_tuple_expr ()->accept_vis (*this);
  }

  void visit (AST::IdentifierExpr &expr) override
  {
    if (!resolver->get_name_scope ().lookup (
	  CanonicalPath::new_seg (expr.get_node_id (), expr.as_string ()),
	  &resolved_node))
      return;

    ok = true;
    // mark the assignment to the name
    resolver->mark_assignment_to_decl (resolved_node, parent);

    // check is mutable
    if (!resolver->decl_is_mutable (resolved_node))
      {
	// we only allow a single assignment to immutable decls
	if (resolver->get_num_assignments_to_decl (resolved_node) > 1)
	  rust_error_at (expr.get_locus (), "cannot assign to immutable");
      }
  }

private:
  VerifyAsignee (NodeId parent) : ResolverBase (parent), ok (false) {}

  bool ok;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_VERIFY_ASSIGNEE
