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
#include "rust-ast-verify-assignee.h"
#include "rust-ast-resolve-type.h"

namespace Rust {
namespace Resolver {

class ResolvePath : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::PathInExpression *expr, NodeId parent)
  {
    ResolvePath resolver (parent);
    resolver.resolve_path (expr);
  }

private:
  ResolvePath (NodeId parent) : ResolverBase (parent) {}

  void resolve_path (AST::PathInExpression *expr);
};

class ResolveExpr : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Expr *expr, NodeId parent)
  {
    ResolveExpr resolver (parent);
    expr->accept_vis (resolver);
  };

  void visit (AST::TupleIndexExpr &expr) override
  {
    ResolveExpr::go (expr.get_tuple_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::TupleExpr &expr) override
  {
    if (expr.is_unit ())
      return;

    for (auto &elem : expr.get_tuple_elems ())
      ResolveExpr::go (elem.get (), expr.get_node_id ());
  }

  void visit (AST::PathInExpression &expr) override
  {
    ResolvePath::go (&expr, parent);
  }

  void visit (AST::ReturnExpr &expr) override
  {
    if (expr.has_returned_expr ())
      ResolveExpr::go (expr.get_returned_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::CallExpr &expr) override
  {
    ResolveExpr::go (expr.get_function_expr ().get (), expr.get_node_id ());
    expr.iterate_params ([&] (AST::Expr *p) mutable -> bool {
      ResolveExpr::go (p, expr.get_node_id ());
      return true;
    });
  }

  void visit (AST::MethodCallExpr &expr) override
  {
    ResolveExpr::go (expr.get_receiver_expr ().get (), expr.get_node_id ());

    if (expr.get_method_name ().has_generic_args ())
      {
	AST::GenericArgs &args = expr.get_method_name ().get_generic_args ();
	ResolveTypeToCanonicalPath::type_resolve_generic_args (args);
      }

    expr.iterate_params ([&] (AST::Expr *p) mutable -> bool {
      ResolveExpr::go (p, expr.get_node_id ());
      return true;
    });
  }

  void visit (AST::AssignmentExpr &expr) override
  {
    ResolveExpr::go (expr.get_left_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_right_expr ().get (), expr.get_node_id ());

    // need to verify the assignee
    VerifyAsignee::go (expr.get_left_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::IdentifierExpr &expr) override
  {
    if (resolver->get_name_scope ().lookup (
	  CanonicalPath::new_seg (expr.get_node_id (), expr.as_string ()),
	  &resolved_node))
      {
	resolver->insert_resolved_name (expr.get_node_id (), resolved_node);
	resolver->insert_new_definition (expr.get_node_id (),
					 Definition{expr.get_node_id (),
						    parent});
      }
    else if (resolver->get_type_scope ().lookup (
	       CanonicalPath::new_seg (expr.get_node_id (), expr.as_string ()),
	       &resolved_node))
      {
	resolver->insert_resolved_type (expr.get_node_id (), resolved_node);
	resolver->insert_new_definition (expr.get_node_id (),
					 Definition{expr.get_node_id (),
						    parent});
      }
    else
      {
	rust_error_at (expr.get_locus (), "failed to find name: %s",
		       expr.as_string ().c_str ());
      }
  }

  void visit (AST::ArithmeticOrLogicalExpr &expr) override
  {
    ResolveExpr::go (expr.get_left_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_right_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::CompoundAssignmentExpr &expr) override
  {
    ResolveExpr::go (expr.get_left_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_right_expr ().get (), expr.get_node_id ());

    // need to verify the assignee
    VerifyAsignee::go (expr.get_left_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::ComparisonExpr &expr) override
  {
    ResolveExpr::go (expr.get_left_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_right_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::LazyBooleanExpr &expr) override
  {
    ResolveExpr::go (expr.get_left_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_right_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::NegationExpr &expr) override
  {
    ResolveExpr::go (expr.get_negated_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::IfExpr &expr) override
  {
    ResolveExpr::go (expr.get_condition_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_if_block ().get (), expr.get_node_id ());
  }

  void visit (AST::IfExprConseqElse &expr) override
  {
    ResolveExpr::go (expr.get_condition_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_if_block ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_else_block ().get (), expr.get_node_id ());
  }

  void visit (AST::IfExprConseqIf &expr) override
  {
    ResolveExpr::go (expr.get_condition_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_if_block ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_conseq_if_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::BlockExpr &expr) override;

  void visit (AST::UnsafeBlockExpr &expr) override
  {
    expr.get_block_expr ()->accept_vis (*this);
  }

  void visit (AST::ArrayElemsValues &elems) override
  {
    elems.iterate ([&] (AST::Expr *elem) mutable -> bool {
      ResolveExpr::go (elem, elems.get_node_id ());
      return true;
    });
  }

  void visit (AST::ArrayExpr &expr) override
  {
    expr.get_array_elems ()->accept_vis (*this);
  }

  void visit (AST::ArrayIndexExpr &expr) override
  {
    ResolveExpr::go (expr.get_array_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_index_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::ArrayElemsCopied &elems) override
  {
    ResolveExpr::go (elems.get_num_copies ().get (), elems.get_node_id ());
    ResolveExpr::go (elems.get_elem_to_copy ().get (), elems.get_node_id ());
  }

  // this this an empty struct constructor like 'S {}'
  void visit (AST::StructExprStruct &struct_expr) override
  {
    ResolveExpr::go (&struct_expr.get_struct_name (),
		     struct_expr.get_node_id ());
  }

  // this this a struct constructor with fields
  void visit (AST::StructExprStructFields &struct_expr) override
  {
    ResolveExpr::go (&struct_expr.get_struct_name (),
		     struct_expr.get_node_id ());

    if (struct_expr.has_struct_base ())
      {
	AST::StructBase &base = struct_expr.get_struct_base ();
	ResolveExpr::go (base.get_base_struct ().get (),
			 struct_expr.get_node_id ());
      }

    struct_expr.iterate (
      [&] (AST::StructExprField *struct_field) mutable -> bool {
	ResolveStructExprField::go (struct_field, struct_expr.get_node_id ());
	return true;
      });
  }

  void visit (AST::GroupedExpr &expr) override
  {
    ResolveExpr::go (expr.get_expr_in_parens ().get (), expr.get_node_id ());
  }

  void visit (AST::FieldAccessExpr &expr) override
  {
    ResolveExpr::go (expr.get_receiver_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::LoopExpr &expr) override
  {
    if (expr.has_loop_label ())
      {
	auto label = expr.get_loop_label ();
	if (label.get_lifetime ().get_lifetime_type ()
	    != AST::Lifetime::LifetimeType::NAMED)
	  {
	    rust_error_at (label.get_locus (),
			   "Labels must be a named lifetime value");
	    return;
	  }

	auto label_name = label.get_lifetime ().get_lifetime_name ();
	auto label_lifetime_node_id = label.get_lifetime ().get_node_id ();
	resolver->get_label_scope ().insert (
	  CanonicalPath::new_seg (expr.get_node_id (), label_name),
	  label_lifetime_node_id, label.get_locus (), false,
	  [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	    rust_error_at (label.get_locus (),
			   "label redefined multiple times");
	    rust_error_at (locus, "was defined here");
	  });
	resolver->insert_new_definition (label_lifetime_node_id,
					 Definition{label_lifetime_node_id,
						    label.get_node_id ()});
      }
    ResolveExpr::go (expr.get_loop_block ().get (), expr.get_node_id ());
  }

  void visit (AST::BreakExpr &expr) override
  {
    if (expr.has_label ())
      {
	auto label = expr.get_label ();
	if (label.get_lifetime_type () != AST::Lifetime::LifetimeType::NAMED)
	  {
	    rust_error_at (label.get_locus (),
			   "Labels must be a named lifetime value");
	    return;
	  }

	NodeId resolved_node = UNKNOWN_NODEID;
	if (!resolver->get_label_scope ().lookup (
	      CanonicalPath::new_seg (label.get_node_id (),
				      label.get_lifetime_name ()),
	      &resolved_node))
	  {
	    rust_error_at (expr.get_label ().get_locus (),
			   "failed to resolve label");
	    return;
	  }
	resolver->insert_resolved_label (label.get_node_id (), resolved_node);
      }

    if (expr.has_break_expr ())
      ResolveExpr::go (expr.get_break_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::WhileLoopExpr &expr) override
  {
    if (expr.has_loop_label ())
      {
	auto label = expr.get_loop_label ();
	if (label.get_lifetime ().get_lifetime_type ()
	    != AST::Lifetime::LifetimeType::NAMED)
	  {
	    rust_error_at (label.get_locus (),
			   "Labels must be a named lifetime value");
	    return;
	  }

	auto label_name = label.get_lifetime ().get_lifetime_name ();
	auto label_lifetime_node_id = label.get_lifetime ().get_node_id ();
	resolver->get_label_scope ().insert (
	  CanonicalPath::new_seg (label.get_node_id (), label_name),
	  label_lifetime_node_id, label.get_locus (), false,
	  [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	    rust_error_at (label.get_locus (),
			   "label redefined multiple times");
	    rust_error_at (locus, "was defined here");
	  });
	resolver->insert_new_definition (label_lifetime_node_id,
					 Definition{label_lifetime_node_id,
						    label.get_node_id ()});
      }
    ResolveExpr::go (expr.get_predicate_expr ().get (), expr.get_node_id ());
    ResolveExpr::go (expr.get_loop_block ().get (), expr.get_node_id ());
  }

  void visit (AST::ContinueExpr &expr) override
  {
    if (expr.has_label ())
      {
	auto label = expr.get_label ();
	if (label.get_lifetime_type () != AST::Lifetime::LifetimeType::NAMED)
	  {
	    rust_error_at (label.get_locus (),
			   "Labels must be a named lifetime value");
	    return;
	  }

	NodeId resolved_node = UNKNOWN_NODEID;
	if (!resolver->get_label_scope ().lookup (
	      CanonicalPath::new_seg (label.get_node_id (),
				      label.get_lifetime_name ()),
	      &resolved_node))
	  {
	    rust_error_at (expr.get_label ().get_locus (),
			   "failed to resolve label");
	    return;
	  }
	resolver->insert_resolved_label (label.get_node_id (), resolved_node);
      }
  }

  void visit (AST::BorrowExpr &expr) override
  {
    ResolveExpr::go (expr.get_borrowed_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::DereferenceExpr &expr) override
  {
    ResolveExpr::go (expr.get_dereferenced_expr ().get (), expr.get_node_id ());
  }

private:
  ResolveExpr (NodeId parent) : ResolverBase (parent) {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_EXPR_H
