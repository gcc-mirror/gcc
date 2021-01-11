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

#ifndef RUST_AST_RESOLVE_EXPR_H
#define RUST_AST_RESOLVE_EXPR_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-full.h"
#include "rust-ast-resolve-struct-expr-field.h"

namespace Rust {
namespace Resolver {

class ResolveExpr : public ResolverBase
{
public:
  static void go (AST::Expr *expr, NodeId parent)
  {
    ResolveExpr resolver (parent);
    expr->accept_vis (resolver);
  };

  ~ResolveExpr () {}

  void visit (AST::PathInExpression &expr)
  {
    // name scope first
    if (resolver->get_name_scope ().lookup (expr.as_string (), &resolved_node))
      {
	resolver->insert_resolved_name (expr.get_node_id (), resolved_node);
	resolver->insert_new_definition (expr.get_node_id (),
					 Definition{expr.get_node_id (),
						    parent});
      }
    // check the type scope
    else if (resolver->get_type_scope ().lookup (expr.as_string (),
						 &resolved_node))
      {
	resolver->insert_resolved_type (expr.get_node_id (), resolved_node);
	resolver->insert_new_definition (expr.get_node_id (),
					 Definition{expr.get_node_id (),
						    parent});
      }
    else
      {
	rust_error_at (expr.get_locus (), "unknown path %s",
		       expr.as_string ().c_str ());
      }
  }

  void visit (AST::ReturnExpr &expr)
  {
    if (expr.has_returned_expr ())
      ResolveExpr::go (expr.get_returned_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::CallExpr &expr)
  {
    ResolveExpr::go (expr.get_function_expr ().get (), expr.get_node_id ());
    expr.iterate_params ([&] (AST::Expr *p) mutable -> bool {
      ResolveExpr::go (p, expr.get_node_id ());
      return true;
    });
    // resolver->insert_resolved_name(NodeId refId,NodeId defId)
  }

  void visit (AST::AssignmentExpr &expr)
  {
    ResolveExpr::go (expr.get_left_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_right_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::IdentifierExpr &expr)
  {
    if (!resolver->get_name_scope ().lookup (expr.as_string (), &resolved_node))
      {
	rust_error_at (expr.get_locus (), "failed to find name: %s",
		       expr.as_string ().c_str ());
	return;
      }

    resolver->insert_resolved_name (expr.get_node_id (), resolved_node);
    resolver->insert_new_definition (expr.get_node_id (),
				     Definition{expr.get_node_id (), parent});
  }

  void visit (AST::ArithmeticOrLogicalExpr &expr)
  {
    ResolveExpr::go (expr.get_left_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_right_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::ComparisonExpr &expr)
  {
    ResolveExpr::go (expr.get_left_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_right_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::LazyBooleanExpr &expr)
  {
    ResolveExpr::go (expr.get_left_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_right_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::IfExpr &expr)
  {
    ResolveExpr::go (expr.get_condition_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_if_block ().get (), expr.get_node_id ());
  }

  void visit (AST::IfExprConseqElse &expr)
  {
    ResolveExpr::go (expr.get_condition_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_if_block ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_else_block ().get (), expr.get_node_id ());
  }

  void visit (AST::IfExprConseqIf &expr)
  {
    ResolveExpr::go (expr.get_condition_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_if_block ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_conseq_if_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::BlockExpr &expr);

  void visit (AST::ArrayElemsValues &elems)
  {
    elems.iterate ([&] (AST::Expr *elem) mutable -> bool {
      ResolveExpr::go (elem, elems.get_node_id ());
      return true;
    });
  }

  void visit (AST::ArrayExpr &expr)
  {
    expr.get_array_elems ()->accept_vis (*this);
  }

  void visit (AST::ArrayIndexExpr &expr)
  {
    ResolveExpr::go (expr.get_array_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_index_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::ArrayElemsCopied &elems)
  {
    ResolveExpr::go (elems.get_num_copies ().get (), elems.get_node_id ());
    ResolveExpr::go (elems.get_elem_to_copy ().get (), elems.get_node_id ());
  }

  void visit (AST::StructExprStructFields &struct_expr)
  {
    ResolveExpr::go (&struct_expr.get_struct_name (),
		     struct_expr.get_node_id ());
    struct_expr.iterate (
      [&] (AST::StructExprField *struct_field) mutable -> bool {
	ResolveStructExprField::go (struct_field, struct_expr.get_node_id ());
	return true;
      });
  }

private:
  ResolveExpr (NodeId parent) : ResolverBase (parent) {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_EXPR_H
