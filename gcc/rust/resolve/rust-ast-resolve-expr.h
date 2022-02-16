// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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
#include "rust-ast-resolve-pattern.h"

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

  static void go (AST::QualifiedPathInExpression *expr, NodeId parent)
  {
    ResolvePath resolver (parent);
    resolver.resolve_path (expr);
  }

private:
  ResolvePath (NodeId parent) : ResolverBase (parent) {}

  void resolve_path (AST::PathInExpression *expr);

  void resolve_path (AST::QualifiedPathInExpression *expr);

  void resolve_segments (CanonicalPath prefix, size_t offs,
			 std::vector<AST::PathExprSegment> &segs,
			 NodeId expr_node_id, Location expr_locus);
};

class ResolveExpr : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Expr *expr, NodeId parent, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix)
  {
    ResolveExpr resolver (parent, prefix, canonical_prefix);
    expr->accept_vis (resolver);
  };

  void visit (AST::MacroInvocation &expr) override
  {
    AST::ASTFragment &fragment = expr.get_fragment ();

    // FIXME
    // this assertion might go away, maybe on failure's to expand a macro?
    rust_assert (!fragment.get_nodes ().empty ());
    fragment.get_nodes ().at (0).accept_vis (*this);
  }

  void visit (AST::TupleIndexExpr &expr) override
  {
    resolve_expr (expr.get_tuple_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::TupleExpr &expr) override
  {
    if (expr.is_unit ())
      return;

    for (auto &elem : expr.get_tuple_elems ())
      resolve_expr (elem.get (), expr.get_node_id ());
  }

  void visit (AST::PathInExpression &expr) override
  {
    ResolvePath::go (&expr, parent);
  }

  void visit (AST::QualifiedPathInExpression &expr) override
  {
    ResolvePath::go (&expr, parent);
  }

  void visit (AST::ReturnExpr &expr) override
  {
    if (expr.has_returned_expr ())
      resolve_expr (expr.get_returned_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::CallExpr &expr) override
  {
    resolve_expr (expr.get_function_expr ().get (), expr.get_node_id ());
    auto const &in_params = expr.get_params ();
    for (auto &param : in_params)
      resolve_expr (param.get (), expr.get_node_id ());
  }

  void visit (AST::MethodCallExpr &expr) override
  {
    resolve_expr (expr.get_receiver_expr ().get (), expr.get_node_id ());

    if (expr.get_method_name ().has_generic_args ())
      {
	AST::GenericArgs &args = expr.get_method_name ().get_generic_args ();
	ResolveTypeToCanonicalPath::type_resolve_generic_args (args);
      }

    auto const &in_params = expr.get_params ();
    for (auto &param : in_params)
      resolve_expr (param.get (), expr.get_node_id ());
  }

  void visit (AST::AssignmentExpr &expr) override
  {
    resolve_expr (expr.get_left_expr ().get (), expr.get_node_id ());
    resolve_expr (expr.get_right_expr ().get (), expr.get_node_id ());

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
    resolve_expr (expr.get_left_expr ().get (), expr.get_node_id ());
    resolve_expr (expr.get_right_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::CompoundAssignmentExpr &expr) override
  {
    resolve_expr (expr.get_left_expr ().get (), expr.get_node_id ());
    resolve_expr (expr.get_right_expr ().get (), expr.get_node_id ());

    // need to verify the assignee
    VerifyAsignee::go (expr.get_left_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::ComparisonExpr &expr) override
  {
    resolve_expr (expr.get_left_expr ().get (), expr.get_node_id ());
    resolve_expr (expr.get_right_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::LazyBooleanExpr &expr) override
  {
    resolve_expr (expr.get_left_expr ().get (), expr.get_node_id ());
    resolve_expr (expr.get_right_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::NegationExpr &expr) override
  {
    resolve_expr (expr.get_negated_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::TypeCastExpr &expr) override
  {
    ResolveType::go (expr.get_type_to_cast_to ().get (), expr.get_node_id ());
    resolve_expr (expr.get_casted_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::IfExpr &expr) override
  {
    resolve_expr (expr.get_condition_expr ().get (), expr.get_node_id ());
    resolve_expr (expr.get_if_block ().get (), expr.get_node_id ());
  }

  void visit (AST::IfExprConseqElse &expr) override
  {
    resolve_expr (expr.get_condition_expr ().get (), expr.get_node_id ());
    resolve_expr (expr.get_if_block ().get (), expr.get_node_id ());
    resolve_expr (expr.get_else_block ().get (), expr.get_node_id ());
  }

  void visit (AST::IfExprConseqIf &expr) override
  {
    resolve_expr (expr.get_condition_expr ().get (), expr.get_node_id ());
    resolve_expr (expr.get_if_block ().get (), expr.get_node_id ());
    resolve_expr (expr.get_conseq_if_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::BlockExpr &expr) override;

  void visit (AST::UnsafeBlockExpr &expr) override
  {
    expr.get_block_expr ()->accept_vis (*this);
  }

  void visit (AST::ArrayElemsValues &elems) override
  {
    for (auto &elem : elems.get_values ())
      resolve_expr (elem.get (), elems.get_node_id ());
  }

  void visit (AST::ArrayExpr &expr) override
  {
    expr.get_array_elems ()->accept_vis (*this);
  }

  void visit (AST::ArrayIndexExpr &expr) override
  {
    resolve_expr (expr.get_array_expr ().get (), expr.get_node_id ());
    resolve_expr (expr.get_index_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::ArrayElemsCopied &elems) override
  {
    resolve_expr (elems.get_num_copies ().get (), elems.get_node_id ());
    resolve_expr (elems.get_elem_to_copy ().get (), elems.get_node_id ());
  }

  // this this an empty struct constructor like 'S {}'
  void visit (AST::StructExprStruct &struct_expr) override
  {
    resolve_expr (&struct_expr.get_struct_name (), struct_expr.get_node_id ());
  }

  // this this a struct constructor with fields
  void visit (AST::StructExprStructFields &struct_expr) override
  {
    resolve_expr (&struct_expr.get_struct_name (), struct_expr.get_node_id ());

    if (struct_expr.has_struct_base ())
      {
	AST::StructBase &base = struct_expr.get_struct_base ();
	resolve_expr (base.get_base_struct ().get (),
		      struct_expr.get_node_id ());
      }

    auto const &struct_fields = struct_expr.get_fields ();
    for (auto &struct_field : struct_fields)
      {
	ResolveStructExprField::go (struct_field.get (),
				    struct_expr.get_node_id (), prefix,
				    canonical_prefix);
      }
  }

  void visit (AST::GroupedExpr &expr) override
  {
    resolve_expr (expr.get_expr_in_parens ().get (), expr.get_node_id ());
  }

  void visit (AST::FieldAccessExpr &expr) override
  {
    resolve_expr (expr.get_receiver_expr ().get (), expr.get_node_id ());
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
    resolve_expr (expr.get_loop_block ().get (), expr.get_node_id ());
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
      resolve_expr (expr.get_break_expr ().get (), expr.get_node_id ());
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
    resolve_expr (expr.get_predicate_expr ().get (), expr.get_node_id ());
    resolve_expr (expr.get_loop_block ().get (), expr.get_node_id ());
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
    resolve_expr (expr.get_borrowed_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::DereferenceExpr &expr) override
  {
    resolve_expr (expr.get_dereferenced_expr ().get (), expr.get_node_id ());
  }

  void visit (AST::MatchExpr &expr) override
  {
    resolve_expr (expr.get_scrutinee_expr ().get (), expr.get_node_id ());
    for (auto &match_case : expr.get_match_cases ())
      {
	// each arm is in its own scope
	NodeId scope_node_id = match_case.get_node_id ();
	resolver->get_name_scope ().push (scope_node_id);
	resolver->get_type_scope ().push (scope_node_id);
	resolver->get_label_scope ().push (scope_node_id);
	resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
	resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
	resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

	// resolve
	AST::MatchArm &arm = match_case.get_arm ();
	if (arm.has_match_arm_guard ())
	  resolve_expr (arm.get_guard_expr ().get (), expr.get_node_id ());

	// insert any possible new patterns
	for (auto &pattern : arm.get_patterns ())
	  {
	    PatternDeclaration::go (pattern.get (), expr.get_node_id ());
	  }

	// resolve the body
	resolve_expr (match_case.get_expr ().get (), expr.get_node_id ());

	// done
	resolver->get_name_scope ().pop ();
	resolver->get_type_scope ().pop ();
	resolver->get_label_scope ().pop ();
      }
  }

protected:
  void resolve_expr (AST::Expr *e, NodeId parent)
  {
    ResolveExpr::go (e, parent, prefix, canonical_prefix);
  }

private:
  ResolveExpr (NodeId parent, const CanonicalPath &prefix,
	       const CanonicalPath &canonical_prefix)
    : ResolverBase (parent), prefix (prefix),
      canonical_prefix (canonical_prefix)
  {}

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_EXPR_H
