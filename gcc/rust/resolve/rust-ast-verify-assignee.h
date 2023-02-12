// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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
  static bool go (AST::Expr *assignee)
  {
    VerifyAsignee checker;
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
  }

  void visit (AST::DereferenceExpr &expr) override
  {
    expr.get_dereferenced_expr ()->accept_vis (*this);
  }

  void visit (AST::PathInExpression &) override { ok = true; }

private:
  VerifyAsignee () : ResolverBase (), ok (false) {}

  bool ok;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_VERIFY_ASSIGNEE
