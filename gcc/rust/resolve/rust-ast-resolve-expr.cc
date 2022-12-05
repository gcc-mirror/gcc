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

#include "rust-ast-resolve-expr.h"
#include "rust-ast-resolve-stmt.h"
#include "rust-ast-resolve-struct-expr-field.h"
#include "rust-ast-verify-assignee.h"
#include "rust-ast-resolve-type.h"
#include "rust-ast-resolve-pattern.h"
#include "rust-ast-resolve-path.h"

namespace Rust {
namespace Resolver {

void
ResolveExpr::go (AST::Expr *expr, const CanonicalPath &prefix,
		 const CanonicalPath &canonical_prefix)
{
  ResolveExpr resolver (prefix, canonical_prefix);
  expr->accept_vis (resolver);
}

void
ResolveExpr::visit (AST::TupleIndexExpr &expr)
{
  ResolveExpr::go (expr.get_tuple_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::TupleExpr &expr)
{
  if (expr.is_unit ())
    return;

  for (auto &elem : expr.get_tuple_elems ())
    ResolveExpr::go (elem.get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::PathInExpression &expr)
{
  ResolvePath::go (&expr);
}

void
ResolveExpr::visit (AST::QualifiedPathInExpression &expr)
{
  ResolvePath::go (&expr);
}

void
ResolveExpr::visit (AST::ReturnExpr &expr)
{
  if (expr.has_returned_expr ())
    ResolveExpr::go (expr.get_returned_expr ().get (), prefix,
		     canonical_prefix);
}

void
ResolveExpr::visit (AST::CallExpr &expr)
{
  ResolveExpr::go (expr.get_function_expr ().get (), prefix, canonical_prefix);
  for (auto &param : expr.get_params ())
    ResolveExpr::go (param.get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::MethodCallExpr &expr)
{
  ResolveExpr::go (expr.get_receiver_expr ().get (), prefix, canonical_prefix);

  if (expr.get_method_name ().has_generic_args ())
    {
      AST::GenericArgs &args = expr.get_method_name ().get_generic_args ();
      ResolveGenericArgs::go (args, prefix, canonical_prefix);
    }

  auto const &in_params = expr.get_params ();
  for (auto &param : in_params)
    ResolveExpr::go (param.get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::AssignmentExpr &expr)
{
  ResolveExpr::go (expr.get_left_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_right_expr ().get (), prefix, canonical_prefix);

  // need to verify the assignee
  VerifyAsignee::go (expr.get_left_expr ().get ());
}

void
ResolveExpr::visit (AST::IdentifierExpr &expr)
{
  if (resolver->get_name_scope ().lookup (
	CanonicalPath::new_seg (expr.get_node_id (), expr.as_string ()),
	&resolved_node))
    {
      resolver->insert_resolved_name (expr.get_node_id (), resolved_node);
    }
  else if (resolver->get_type_scope ().lookup (
	     CanonicalPath::new_seg (expr.get_node_id (), expr.as_string ()),
	     &resolved_node))
    {
      resolver->insert_resolved_type (expr.get_node_id (), resolved_node);
    }
  else
    {
      rust_error_at (expr.get_locus (), "failed to find name: %s",
		     expr.as_string ().c_str ());
    }
}

void
ResolveExpr::visit (AST::ArithmeticOrLogicalExpr &expr)
{
  ResolveExpr::go (expr.get_left_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_right_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::CompoundAssignmentExpr &expr)
{
  ResolveExpr::go (expr.get_left_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_right_expr ().get (), prefix, canonical_prefix);

  // need to verify the assignee
  VerifyAsignee::go (expr.get_left_expr ().get ());
}

void
ResolveExpr::visit (AST::ComparisonExpr &expr)
{
  ResolveExpr::go (expr.get_left_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_right_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::LazyBooleanExpr &expr)
{
  ResolveExpr::go (expr.get_left_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_right_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::NegationExpr &expr)
{
  ResolveExpr::go (expr.get_negated_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::TypeCastExpr &expr)
{
  ResolveType::go (expr.get_type_to_cast_to ().get ());
  ResolveExpr::go (expr.get_casted_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::IfExpr &expr)
{
  ResolveExpr::go (expr.get_condition_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_if_block ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::IfExprConseqElse &expr)
{
  ResolveExpr::go (expr.get_condition_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_if_block ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_else_block ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::IfExprConseqIf &expr)
{
  ResolveExpr::go (expr.get_condition_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_if_block ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_conseq_if_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::IfLetExpr &expr)
{
  ResolveExpr::go (expr.get_value_expr ().get (), prefix, canonical_prefix);

  NodeId scope_node_id = expr.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  for (auto &pattern : expr.get_patterns ())
    {
      PatternDeclaration::go (pattern.get (), Rib::ItemType::Var);
    }

  ResolveExpr::go (expr.get_if_block ().get (), prefix, canonical_prefix);

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveExpr::visit (AST::BlockExpr &expr)
{
  NodeId scope_node_id = expr.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  for (auto &s : expr.get_statements ())
    {
      if (s->is_item ())
	ResolveStmt::go (s.get (), prefix, canonical_prefix,
			 CanonicalPath::create_empty ());
    }

  for (auto &s : expr.get_statements ())
    {
      if (!s->is_item ())
	ResolveStmt::go (s.get (), prefix, canonical_prefix,
			 CanonicalPath::create_empty ());
    }

  if (expr.has_tail_expr ())
    ResolveExpr::go (expr.get_tail_expr ().get (), prefix, canonical_prefix);

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveExpr::visit (AST::UnsafeBlockExpr &expr)
{
  expr.get_block_expr ()->accept_vis (*this);
}

void
ResolveExpr::visit (AST::ArrayElemsValues &elems)
{
  for (auto &elem : elems.get_values ())
    ResolveExpr::go (elem.get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::ArrayExpr &expr)
{
  expr.get_array_elems ()->accept_vis (*this);
}

void
ResolveExpr::visit (AST::ArrayIndexExpr &expr)
{
  ResolveExpr::go (expr.get_array_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_index_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::ArrayElemsCopied &expr)
{
  ResolveExpr::go (expr.get_num_copies ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_elem_to_copy ().get (), prefix, canonical_prefix);
}

// this this an empty struct constructor like 'S {}'
void
ResolveExpr::visit (AST::StructExprStruct &struct_expr)
{
  ResolveExpr::go (&struct_expr.get_struct_name (), prefix, canonical_prefix);
}

// this this a struct constructor with fields
void
ResolveExpr::visit (AST::StructExprStructFields &struct_expr)
{
  ResolveExpr::go (&struct_expr.get_struct_name (), prefix, canonical_prefix);

  if (struct_expr.has_struct_base ())
    {
      AST::StructBase &base = struct_expr.get_struct_base ();
      ResolveExpr::go (base.get_base_struct ().get (), prefix,
		       canonical_prefix);
    }

  auto const &struct_fields = struct_expr.get_fields ();
  for (auto &struct_field : struct_fields)
    {
      ResolveStructExprField::go (struct_field.get (), prefix,
				  canonical_prefix);
    }
}

void
ResolveExpr::visit (AST::GroupedExpr &expr)
{
  ResolveExpr::go (expr.get_expr_in_parens ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::FieldAccessExpr &expr)
{
  ResolveExpr::go (expr.get_receiver_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::LoopExpr &expr)
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
	label_lifetime_node_id, label.get_locus (), false, Rib::ItemType::Label,
	[&] (const CanonicalPath &, NodeId, Location locus) -> void {
	  rust_error_at (label.get_locus (), "label redefined multiple times");
	  rust_error_at (locus, "was defined here");
	});
    }
  ResolveExpr::go (expr.get_loop_block ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::BreakExpr &expr)
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
    ResolveExpr::go (expr.get_break_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::WhileLoopExpr &expr)
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
	label_lifetime_node_id, label.get_locus (), false, Rib::ItemType::Label,
	[&] (const CanonicalPath &, NodeId, Location locus) -> void {
	  rust_error_at (label.get_locus (), "label redefined multiple times");
	  rust_error_at (locus, "was defined here");
	});
    }

  ResolveExpr::go (expr.get_predicate_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_loop_block ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::ForLoopExpr &expr)
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
	label_lifetime_node_id, label.get_locus (), false, Rib::ItemType::Label,
	[&] (const CanonicalPath &, NodeId, Location locus) -> void {
	  rust_error_at (label.get_locus (), "label redefined multiple times");
	  rust_error_at (locus, "was defined here");
	});
    }

  // this needs a new rib to contain the pattern
  NodeId scope_node_id = expr.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  // resolve the expression
  PatternDeclaration::go (expr.get_pattern ().get (), Rib::ItemType::Var);
  ResolveExpr::go (expr.get_iterator_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_loop_block ().get (), prefix, canonical_prefix);

  // done
  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveExpr::visit (AST::ContinueExpr &expr)
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

void
ResolveExpr::visit (AST::BorrowExpr &expr)
{
  ResolveExpr::go (expr.get_borrowed_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::DereferenceExpr &expr)
{
  ResolveExpr::go (expr.get_dereferenced_expr ().get (), prefix,
		   canonical_prefix);
}

void
ResolveExpr::visit (AST::MatchExpr &expr)
{
  ResolveExpr::go (expr.get_scrutinee_expr ().get (), prefix, canonical_prefix);
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
	ResolveExpr::go (arm.get_guard_expr ().get (), prefix,
			 canonical_prefix);

      // insert any possible new patterns
      for (auto &pattern : arm.get_patterns ())
	{
	  PatternDeclaration::go (pattern.get (), Rib::ItemType::Var);
	}

      // resolve the body
      ResolveExpr::go (match_case.get_expr ().get (), prefix, canonical_prefix);

      // done
      resolver->get_name_scope ().pop ();
      resolver->get_type_scope ().pop ();
      resolver->get_label_scope ().pop ();
    }
}

void
ResolveExpr::visit (AST::RangeFromToExpr &expr)
{
  ResolveExpr::go (expr.get_from_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_to_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::RangeFromExpr &expr)
{
  ResolveExpr::go (expr.get_from_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::RangeToExpr &expr)
{
  ResolveExpr::go (expr.get_to_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::RangeFullExpr &)
{
  // nothing to do
}

void
ResolveExpr::visit (AST::RangeFromToInclExpr &expr)
{
  ResolveExpr::go (expr.get_from_expr ().get (), prefix, canonical_prefix);
  ResolveExpr::go (expr.get_to_expr ().get (), prefix, canonical_prefix);
}

void
ResolveExpr::visit (AST::ClosureExprInner &expr)
{
  NodeId scope_node_id = expr.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  for (auto &p : expr.get_params ())
    {
      resolve_closure_param (p);
    }

  resolver->push_closure_context (expr.get_node_id ());

  ResolveExpr::go (expr.get_definition_expr ().get (), prefix,
		   canonical_prefix);

  resolver->pop_closure_context ();

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveExpr::visit (AST::ClosureExprInnerTyped &expr)
{
  NodeId scope_node_id = expr.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  for (auto &p : expr.get_params ())
    {
      resolve_closure_param (p);
    }

  ResolveType::go (expr.get_return_type ().get ());

  resolver->push_closure_context (expr.get_node_id ());

  ResolveExpr::go (expr.get_definition_block ().get (), prefix,
		   canonical_prefix);

  resolver->pop_closure_context ();

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveExpr::resolve_closure_param (AST::ClosureParam &param)
{
  PatternDeclaration::go (param.get_pattern ().get (), Rib::ItemType::Param);

  if (param.has_type_given ())
    ResolveType::go (param.get_type ().get ());
}

ResolveExpr::ResolveExpr (const CanonicalPath &prefix,
			  const CanonicalPath &canonical_prefix)
  : ResolverBase (), prefix (prefix), canonical_prefix (canonical_prefix)
{}

} // namespace Resolver
} // namespace Rust
