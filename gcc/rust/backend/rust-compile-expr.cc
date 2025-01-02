// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-compile-expr.h"
#include "rust-compile-struct-field-expr.h"
#include "rust-compile-pattern.h"
#include "rust-compile-resolve-path.h"
#include "rust-compile-block.h"
#include "rust-compile-implitem.h"
#include "rust-constexpr.h"
#include "rust-compile-type.h"
#include "rust-gcc.h"

#include "fold-const.h"
#include "realmpfr.h"
#include "convert.h"
#include "print-tree.h"

namespace Rust {
namespace Compile {

CompileExpr::CompileExpr (Context *ctx)
  : HIRCompileBase (ctx), translated (error_mark_node)
{}

tree
CompileExpr::Compile (HIR::Expr *expr, Context *ctx)
{
  CompileExpr compiler (ctx);
  expr->accept_vis (compiler);
  return compiler.translated;
}

void
CompileExpr::visit (HIR::TupleIndexExpr &expr)
{
  HIR::Expr *tuple_expr = expr.get_tuple_expr ().get ();
  TupleIndex index = expr.get_tuple_index ();

  tree receiver_ref = CompileExpr::Compile (tuple_expr, ctx);

  TyTy::BaseType *tuple_expr_ty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (tuple_expr->get_mappings ().get_hirid (),
				      &tuple_expr_ty);
  rust_assert (ok);

  // do we need to add an indirect reference
  if (tuple_expr_ty->get_kind () == TyTy::TypeKind::REF)
    {
      tree indirect = indirect_expression (receiver_ref, expr.get_locus ());
      receiver_ref = indirect;
    }

  translated
    = Backend::struct_field_expression (receiver_ref, index, expr.get_locus ());
}

void
CompileExpr::visit (HIR::TupleExpr &expr)
{
  if (expr.is_unit ())
    {
      translated = unit_expression (ctx, expr.get_locus ());
      return;
    }

  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &tyty))
    {
      rust_fatal_error (expr.get_locus (),
			"did not resolve type for this TupleExpr");
      return;
    }

  tree tuple_type = TyTyResolveCompile::compile (ctx, tyty);
  rust_assert (tuple_type != nullptr);

  // this assumes all fields are in order from type resolution
  std::vector<tree> vals;
  for (auto &elem : expr.get_tuple_elems ())
    {
      auto e = CompileExpr::Compile (elem.get (), ctx);
      vals.push_back (e);
    }

  translated = Backend::constructor_expression (tuple_type, false, vals, -1,
						expr.get_locus ());
}

void
CompileExpr::visit (HIR::ReturnExpr &expr)
{
  auto fncontext = ctx->peek_fn ();

  tree return_value = expr.has_return_expr ()
			? CompileExpr::Compile (expr.return_expr.get (), ctx)
			: unit_expression (ctx, expr.get_locus ());

  if (expr.has_return_expr ())
    {
      HirId id = expr.get_mappings ().get_hirid ();
      location_t rvalue_locus = expr.return_expr->get_locus ();

      TyTy::BaseType *expected = fncontext.retty;
      location_t lvalue_locus
	= ctx->get_mappings ()->lookup_location (expected->get_ref ());

      TyTy::BaseType *actual = nullptr;
      bool ok = ctx->get_tyctx ()->lookup_type (
	expr.return_expr->get_mappings ().get_hirid (), &actual);
      rust_assert (ok);

      return_value = coercion_site (id, return_value, actual, expected,
				    lvalue_locus, rvalue_locus);
    }

  tree return_stmt = Backend::return_statement (fncontext.fndecl, return_value,
						expr.get_locus ());
  ctx->add_statement (return_stmt);
}

void
CompileExpr::visit (HIR::ArithmeticOrLogicalExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto lhs = CompileExpr::Compile (expr.get_lhs ().get (), ctx);
  auto rhs = CompileExpr::Compile (expr.get_rhs ().get (), ctx);

  // this might be an operator overload situation lets check
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type
	= LangItem::OperatorToLangItem (expr.get_expr_type ());
      translated = resolve_operator_overload (lang_item_type, expr, lhs, rhs,
					      expr.get_lhs ().get (),
					      expr.get_rhs ().get ());
      return;
    }

  if (ctx->in_fn () && !ctx->const_context_p ())
    {
      auto receiver_tmp = NULL_TREE;
      auto receiver
	= Backend::temporary_variable (ctx->peek_fn ().fndecl, NULL_TREE,
				       TREE_TYPE (lhs), lhs, true,
				       expr.get_locus (), &receiver_tmp);
      auto check
	= Backend::arithmetic_or_logical_expression_checked (op, lhs, rhs,
							     expr.get_locus (),
							     receiver);

      ctx->add_statement (check);
      translated = receiver->get_tree (expr.get_locus ());
    }
  else
    {
      translated
	= Backend::arithmetic_or_logical_expression (op, lhs, rhs,
						     expr.get_locus ());
    }
}

void
CompileExpr::visit (HIR::CompoundAssignmentExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto lhs = CompileExpr::Compile (expr.get_lhs ().get (), ctx);
  auto rhs = CompileExpr::Compile (expr.get_rhs ().get (), ctx);

  // this might be an operator overload situation lets check
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type = LangItem::CompoundAssignmentOperatorToLangItem (
	expr.get_expr_type ());
      auto compound_assignment
	= resolve_operator_overload (lang_item_type, expr, lhs, rhs,
				     expr.get_lhs ().get (),
				     expr.get_rhs ().get ());
      ctx->add_statement (compound_assignment);

      return;
    }

  if (ctx->in_fn () && !ctx->const_context_p ())
    {
      auto tmp = NULL_TREE;
      auto receiver
	= Backend::temporary_variable (ctx->peek_fn ().fndecl, NULL_TREE,
				       TREE_TYPE (lhs), lhs, true,
				       expr.get_locus (), &tmp);
      auto check
	= Backend::arithmetic_or_logical_expression_checked (op, lhs, rhs,
							     expr.get_locus (),
							     receiver);
      ctx->add_statement (check);

      translated
	= Backend::assignment_statement (lhs,
					 receiver->get_tree (expr.get_locus ()),
					 expr.get_locus ());
    }
  else
    {
      translated
	= Backend::arithmetic_or_logical_expression (op, lhs, rhs,
						     expr.get_locus ());
    }
}

void
CompileExpr::visit (HIR::NegationExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto negated_expr = CompileExpr::Compile (expr.get_expr ().get (), ctx);
  auto location = expr.get_locus ();

  // this might be an operator overload situation lets check
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type = LangItem::NegationOperatorToLangItem (op);
      translated
	= resolve_operator_overload (lang_item_type, expr, negated_expr,
				     nullptr, expr.get_expr ().get (), nullptr);
      return;
    }

  translated = Backend::negation_expression (op, negated_expr, location);
}

void
CompileExpr::visit (HIR::ComparisonExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto lhs = CompileExpr::Compile (expr.get_lhs ().get (), ctx);
  auto rhs = CompileExpr::Compile (expr.get_rhs ().get (), ctx);
  auto location = expr.get_locus ();

  translated = Backend::comparison_expression (op, lhs, rhs, location);
}

void
CompileExpr::visit (HIR::LazyBooleanExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto lhs = CompileExpr::Compile (expr.get_lhs ().get (), ctx);
  auto rhs = CompileExpr::Compile (expr.get_rhs ().get (), ctx);
  auto location = expr.get_locus ();

  translated = Backend::lazy_boolean_expression (op, lhs, rhs, location);
}

void
CompileExpr::visit (HIR::TypeCastExpr &expr)
{
  TyTy::BaseType *type_to_cast_to_ty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &type_to_cast_to_ty))
    {
      translated = error_mark_node;
      return;
    }

  TyTy::BaseType *casted_tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (
	expr.get_casted_expr ()->get_mappings ().get_hirid (), &casted_tyty))
    {
      translated = error_mark_node;
      return;
    }

  auto type_to_cast_to = TyTyResolveCompile::compile (ctx, type_to_cast_to_ty);
  auto casted_expr = CompileExpr::Compile (expr.get_casted_expr ().get (), ctx);

  std::vector<Resolver::Adjustment> *adjustments = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_cast_autoderef_mappings (
    expr.get_mappings ().get_hirid (), &adjustments);
  if (ok)
    {
      casted_expr
	= resolve_adjustements (*adjustments, casted_expr, expr.get_locus ());
    }

  translated
    = type_cast_expression (type_to_cast_to, casted_expr, expr.get_locus ());
}

void
CompileExpr::visit (HIR::IfExpr &expr)
{
  auto stmt = CompileConditionalBlocks::compile (&expr, ctx, nullptr);
  ctx->add_statement (stmt);
}

void
CompileExpr::visit (HIR::IfExprConseqElse &expr)
{
  TyTy::BaseType *if_type = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &if_type))
    {
      rust_error_at (expr.get_locus (),
		     "failed to lookup type of IfExprConseqElse");
      return;
    }

  Bvariable *tmp = NULL;
  fncontext fnctx = ctx->peek_fn ();
  tree enclosing_scope = ctx->peek_enclosing_scope ();
  tree block_type = TyTyResolveCompile::compile (ctx, if_type);

  bool is_address_taken = false;
  tree ret_var_stmt = nullptr;
  tmp = Backend::temporary_variable (fnctx.fndecl, enclosing_scope, block_type,
				     NULL, is_address_taken, expr.get_locus (),
				     &ret_var_stmt);
  ctx->add_statement (ret_var_stmt);

  auto stmt = CompileConditionalBlocks::compile (&expr, ctx, tmp);
  ctx->add_statement (stmt);

  translated = Backend::var_expression (tmp, expr.get_locus ());
}

void
CompileExpr::visit (HIR::BlockExpr &expr)
{
  if (expr.has_label ())
    {
      rust_error_at (expr.get_locus (), "labeled blocks are not supported");
      return;
    }

  TyTy::BaseType *block_tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &block_tyty))
    {
      rust_error_at (expr.get_locus (), "failed to lookup type of BlockExpr");
      return;
    }

  Bvariable *tmp = NULL;
  fncontext fnctx = ctx->peek_fn ();
  tree enclosing_scope = ctx->peek_enclosing_scope ();
  tree block_type = TyTyResolveCompile::compile (ctx, block_tyty);

  bool is_address_taken = false;
  tree ret_var_stmt = nullptr;
  tmp = Backend::temporary_variable (fnctx.fndecl, enclosing_scope, block_type,
				     NULL, is_address_taken, expr.get_locus (),
				     &ret_var_stmt);
  ctx->add_statement (ret_var_stmt);

  auto block_stmt = CompileBlock::compile (&expr, ctx, tmp);
  rust_assert (TREE_CODE (block_stmt) == BIND_EXPR);
  ctx->add_statement (block_stmt);

  translated = Backend::var_expression (tmp, expr.get_locus ());
}

void
CompileExpr::visit (HIR::UnsafeBlockExpr &expr)
{
  expr.get_block_expr ()->accept_vis (*this);
}

void
CompileExpr::visit (HIR::StructExprStruct &struct_expr)
{
  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (struct_expr.get_mappings ().get_hirid (),
				       &tyty))
    {
      rust_error_at (struct_expr.get_locus (), "unknown type");
      return;
    }

  rust_assert (tyty->is_unit ());
  translated = unit_expression (ctx, struct_expr.get_locus ());
}

void
CompileExpr::visit (HIR::StructExprStructFields &struct_expr)
{
  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (struct_expr.get_mappings ().get_hirid (),
				       &tyty))
    {
      rust_error_at (struct_expr.get_locus (), "unknown type");
      return;
    }

  // it must be an ADT
  rust_assert (tyty->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (tyty);

  // what variant is it?
  int union_disriminator = struct_expr.union_index;
  TyTy::VariantDef *variant = nullptr;
  if (!adt->is_enum ())
    {
      rust_assert (adt->number_of_variants () == 1);
      variant = adt->get_variants ().at (0);
    }
  else
    {
      HirId variant_id;
      bool ok = ctx->get_tyctx ()->lookup_variant_definition (
	struct_expr.get_struct_name ().get_mappings ().get_hirid (),
	&variant_id);
      rust_assert (ok);

      ok
	= adt->lookup_variant_by_id (variant_id, &variant, &union_disriminator);
      rust_assert (ok);
    }

  // compile it
  tree compiled_adt_type = TyTyResolveCompile::compile (ctx, tyty);

  std::vector<tree> arguments;
  if (adt->is_union ())
    {
      rust_assert (struct_expr.get_fields ().size () == 1);

      // assignments are coercion sites so lets convert the rvalue if
      // necessary
      auto respective_field = variant->get_field_at_index (union_disriminator);
      auto expected = respective_field->get_field_type ();

      // process arguments
      auto &argument = struct_expr.get_fields ().at (0);
      auto lvalue_locus
	= ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
      auto rvalue_locus = argument->get_locus ();
      auto rvalue = CompileStructExprField::Compile (argument.get (), ctx);

      TyTy::BaseType *actual = nullptr;
      bool ok = ctx->get_tyctx ()->lookup_type (
	argument->get_mappings ().get_hirid (), &actual);

      if (ok)
	{
	  rvalue
	    = coercion_site (argument->get_mappings ().get_hirid (), rvalue,
			     actual, expected, lvalue_locus, rvalue_locus);
	}

      // add it to the list
      arguments.push_back (rvalue);
    }
  else
    {
      // this assumes all fields are in order from type resolution and if a
      // base struct was specified those fields are filed via accessors
      for (size_t i = 0; i < struct_expr.get_fields ().size (); i++)
	{
	  // assignments are coercion sites so lets convert the rvalue if
	  // necessary
	  auto respective_field = variant->get_field_at_index (i);
	  auto expected = respective_field->get_field_type ();

	  // process arguments
	  auto &argument = struct_expr.get_fields ().at (i);
	  auto lvalue_locus
	    = ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
	  auto rvalue_locus = argument->get_locus ();
	  auto rvalue = CompileStructExprField::Compile (argument.get (), ctx);

	  TyTy::BaseType *actual = nullptr;
	  bool ok = ctx->get_tyctx ()->lookup_type (
	    argument->get_mappings ().get_hirid (), &actual);

	  // coerce it if required/possible see
	  // compile/torture/struct_base_init_1.rs
	  if (ok)
	    {
	      rvalue
		= coercion_site (argument->get_mappings ().get_hirid (), rvalue,
				 actual, expected, lvalue_locus, rvalue_locus);
	    }

	  // add it to the list
	  arguments.push_back (rvalue);
	}
    }

  // the constructor depends on whether this is actually an enum or not if
  // its an enum we need to setup the discriminator
  std::vector<tree> ctor_arguments;
  if (adt->is_enum ())
    {
      HIR::Expr *discrim_expr = variant->get_discriminant ();
      tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);
      tree folded_discrim_expr = fold_expr (discrim_expr_node);
      tree qualifier = folded_discrim_expr;

      ctor_arguments.push_back (qualifier);
    }
  for (auto &arg : arguments)
    ctor_arguments.push_back (arg);

  translated
    = Backend::constructor_expression (compiled_adt_type, adt->is_enum (),
				       ctor_arguments, union_disriminator,
				       struct_expr.get_locus ());
}

void
CompileExpr::visit (HIR::GroupedExpr &expr)
{
  translated = CompileExpr::Compile (expr.get_expr_in_parens ().get (), ctx);
}

void
CompileExpr::visit (HIR::FieldAccessExpr &expr)
{
  HIR::Expr *receiver_expr = expr.get_receiver_expr ().get ();
  tree receiver_ref = CompileExpr::Compile (receiver_expr, ctx);

  // resolve the receiver back to ADT type
  TyTy::BaseType *receiver = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (
	expr.get_receiver_expr ()->get_mappings ().get_hirid (), &receiver))
    {
      rust_error_at (expr.get_receiver_expr ()->get_locus (),
		     "unresolved type for receiver");
      return;
    }

  size_t field_index = 0;
  if (receiver->get_kind () == TyTy::TypeKind::ADT)
    {
      TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (receiver);
      rust_assert (!adt->is_enum ());
      rust_assert (adt->number_of_variants () == 1);

      TyTy::VariantDef *variant = adt->get_variants ().at (0);
      bool ok = variant->lookup_field (expr.get_field_name ().as_string (),
				       nullptr, &field_index);
      rust_assert (ok);
    }
  else if (receiver->get_kind () == TyTy::TypeKind::REF)
    {
      TyTy::ReferenceType *r = static_cast<TyTy::ReferenceType *> (receiver);
      TyTy::BaseType *b = r->get_base ();
      rust_assert (b->get_kind () == TyTy::TypeKind::ADT);

      TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (b);
      rust_assert (!adt->is_enum ());
      rust_assert (adt->number_of_variants () == 1);

      TyTy::VariantDef *variant = adt->get_variants ().at (0);
      bool ok = variant->lookup_field (expr.get_field_name ().as_string (),
				       nullptr, &field_index);
      rust_assert (ok);

      tree indirect = indirect_expression (receiver_ref, expr.get_locus ());
      receiver_ref = indirect;
    }

  translated = Backend::struct_field_expression (receiver_ref, field_index,
						 expr.get_locus ());
}

void
CompileExpr::visit (HIR::QualifiedPathInExpression &expr)
{
  translated = ResolvePathRef::Compile (expr, ctx);
}

void
CompileExpr::visit (HIR::PathInExpression &expr)
{
  translated = ResolvePathRef::Compile (expr, ctx);
}

void
CompileExpr::visit (HIR::LoopExpr &expr)
{
  TyTy::BaseType *block_tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &block_tyty))
    {
      rust_error_at (expr.get_locus (), "failed to lookup type of BlockExpr");
      return;
    }

  fncontext fnctx = ctx->peek_fn ();
  tree enclosing_scope = ctx->peek_enclosing_scope ();
  tree block_type = TyTyResolveCompile::compile (ctx, block_tyty);

  bool is_address_taken = false;
  tree ret_var_stmt = NULL_TREE;
  Bvariable *tmp
    = Backend::temporary_variable (fnctx.fndecl, enclosing_scope, block_type,
				   NULL, is_address_taken, expr.get_locus (),
				   &ret_var_stmt);
  ctx->add_statement (ret_var_stmt);
  ctx->push_loop_context (tmp);

  if (expr.has_loop_label ())
    {
      HIR::LoopLabel &loop_label = expr.get_loop_label ();
      tree label
	= Backend::label (fnctx.fndecl, loop_label.get_lifetime ().get_name (),
			  loop_label.get_locus ());
      tree label_decl = Backend::label_definition_statement (label);
      ctx->add_statement (label_decl);
      ctx->insert_label_decl (
	loop_label.get_lifetime ().get_mappings ().get_hirid (), label);
    }

  tree loop_begin_label = Backend::label (fnctx.fndecl, "", expr.get_locus ());
  tree loop_begin_label_decl
    = Backend::label_definition_statement (loop_begin_label);
  ctx->add_statement (loop_begin_label_decl);
  ctx->push_loop_begin_label (loop_begin_label);

  tree code_block
    = CompileBlock::compile (expr.get_loop_block ().get (), ctx, nullptr);
  tree loop_expr = Backend::loop_expression (code_block, expr.get_locus ());
  ctx->add_statement (loop_expr);

  ctx->pop_loop_context ();
  translated = Backend::var_expression (tmp, expr.get_locus ());

  ctx->pop_loop_begin_label ();
}

void
CompileExpr::visit (HIR::WhileLoopExpr &expr)
{
  fncontext fnctx = ctx->peek_fn ();
  if (expr.has_loop_label ())
    {
      HIR::LoopLabel &loop_label = expr.get_loop_label ();
      tree label
	= Backend::label (fnctx.fndecl, loop_label.get_lifetime ().get_name (),
			  loop_label.get_locus ());
      tree label_decl = Backend::label_definition_statement (label);
      ctx->add_statement (label_decl);
      ctx->insert_label_decl (
	loop_label.get_lifetime ().get_mappings ().get_hirid (), label);
    }

  std::vector<Bvariable *> locals;
  location_t start_location = expr.get_loop_block ()->get_locus ();
  location_t end_location = expr.get_loop_block ()->get_locus (); // FIXME

  tree enclosing_scope = ctx->peek_enclosing_scope ();
  tree loop_block = Backend::block (fnctx.fndecl, enclosing_scope, locals,
				    start_location, end_location);
  ctx->push_block (loop_block);

  tree loop_begin_label = Backend::label (fnctx.fndecl, "", expr.get_locus ());
  tree loop_begin_label_decl
    = Backend::label_definition_statement (loop_begin_label);
  ctx->add_statement (loop_begin_label_decl);
  ctx->push_loop_begin_label (loop_begin_label);

  tree condition
    = CompileExpr::Compile (expr.get_predicate_expr ().get (), ctx);
  tree exit_condition = fold_build1_loc (expr.get_locus (), TRUTH_NOT_EXPR,
					 boolean_type_node, condition);
  tree exit_expr = Backend::exit_expression (exit_condition, expr.get_locus ());
  ctx->add_statement (exit_expr);

  tree code_block_stmt
    = CompileBlock::compile (expr.get_loop_block ().get (), ctx, nullptr);
  rust_assert (TREE_CODE (code_block_stmt) == BIND_EXPR);
  ctx->add_statement (code_block_stmt);

  ctx->pop_loop_begin_label ();
  ctx->pop_block ();

  tree loop_expr = Backend::loop_expression (loop_block, expr.get_locus ());
  ctx->add_statement (loop_expr);
}

void
CompileExpr::visit (HIR::BreakExpr &expr)
{
  if (expr.has_break_expr ())
    {
      tree compiled_expr = CompileExpr::Compile (expr.get_expr ().get (), ctx);

      Bvariable *loop_result_holder = ctx->peek_loop_context ();
      tree result_reference
	= Backend::var_expression (loop_result_holder,
				   expr.get_expr ()->get_locus ());

      tree assignment
	= Backend::assignment_statement (result_reference, compiled_expr,
					 expr.get_locus ());
      ctx->add_statement (assignment);
    }

  if (expr.has_label ())
    {
      NodeId resolved_node_id = UNKNOWN_NODEID;
      if (!ctx->get_resolver ()->lookup_resolved_label (
	    expr.get_label ().get_mappings ().get_nodeid (), &resolved_node_id))
	{
	  rust_error_at (
	    expr.get_label ().get_locus (),
	    "failed to resolve compiled label for label %s",
	    expr.get_label ().get_mappings ().as_string ().c_str ());
	  return;
	}

      HirId ref = UNKNOWN_HIRID;
      if (!ctx->get_mappings ()->lookup_node_to_hir (resolved_node_id, &ref))
	{
	  rust_fatal_error (expr.get_locus (), "reverse lookup label failure");
	  return;
	}

      tree label = NULL_TREE;
      if (!ctx->lookup_label_decl (ref, &label))
	{
	  rust_error_at (expr.get_label ().get_locus (),
			 "failed to lookup compiled label");
	  return;
	}

      tree goto_label = Backend::goto_statement (label, expr.get_locus ());
      ctx->add_statement (goto_label);
    }
  else
    {
      tree exit_expr
	= Backend::exit_expression (Backend::boolean_constant_expression (true),
				    expr.get_locus ());
      ctx->add_statement (exit_expr);
    }
}

void
CompileExpr::visit (HIR::ContinueExpr &expr)
{
  tree label = ctx->peek_loop_begin_label ();
  if (expr.has_label ())
    {
      NodeId resolved_node_id = UNKNOWN_NODEID;
      if (!ctx->get_resolver ()->lookup_resolved_label (
	    expr.get_label ().get_mappings ().get_nodeid (), &resolved_node_id))
	{
	  rust_error_at (
	    expr.get_label ().get_locus (),
	    "failed to resolve compiled label for label %s",
	    expr.get_label ().get_mappings ().as_string ().c_str ());
	  return;
	}

      HirId ref = UNKNOWN_HIRID;
      if (!ctx->get_mappings ()->lookup_node_to_hir (resolved_node_id, &ref))
	{
	  rust_fatal_error (expr.get_locus (), "reverse lookup label failure");
	  return;
	}

      if (!ctx->lookup_label_decl (ref, &label))
	{
	  rust_error_at (expr.get_label ().get_locus (),
			 "failed to lookup compiled label");
	  return;
	}
    }

  translated = Backend::goto_statement (label, expr.get_locus ());
}

void
CompileExpr::visit (HIR::BorrowExpr &expr)
{
  tree main_expr = CompileExpr::Compile (expr.get_expr ().get (), ctx);
  if (RS_DST_FLAG_P (TREE_TYPE (main_expr)))
    {
      translated = main_expr;
      return;
    }

  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &tyty))
    return;

  translated = address_expression (main_expr, expr.get_locus ());
}

void
CompileExpr::visit (HIR::DereferenceExpr &expr)
{
  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &tyty))
    {
      rust_fatal_error (expr.get_locus (),
			"did not resolve type for this TupleExpr");
      return;
    }

  tree main_expr = CompileExpr::Compile (expr.get_expr ().get (), ctx);

  // this might be an operator overload situation lets check
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type = LangItem::Kind::DEREF;
      tree operator_overload_call
	= resolve_operator_overload (lang_item_type, expr, main_expr, nullptr,
				     expr.get_expr ().get (), nullptr);

      // rust deref always returns a reference from this overload then we can
      // actually do the indirection
      main_expr = operator_overload_call;
    }

  tree expected_type = TyTyResolveCompile::compile (ctx, tyty);
  if (RS_DST_FLAG_P (TREE_TYPE (main_expr)) && RS_DST_FLAG_P (expected_type))
    {
      translated = main_expr;
      return;
    }

  translated = indirect_expression (main_expr, expr.get_locus ());
}

void
CompileExpr::visit (HIR::LiteralExpr &expr)
{
  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &tyty))
    return;

  switch (expr.get_lit_type ())
    {
    case HIR::Literal::BOOL:
      translated = compile_bool_literal (expr, tyty);
      return;

    case HIR::Literal::INT:
      translated = compile_integer_literal (expr, tyty);
      return;

    case HIR::Literal::FLOAT:
      translated = compile_float_literal (expr, tyty);
      return;

    case HIR::Literal::CHAR:
      translated = compile_char_literal (expr, tyty);
      return;

    case HIR::Literal::BYTE:
      translated = compile_byte_literal (expr, tyty);
      return;

    case HIR::Literal::STRING:
      translated = compile_string_literal (expr, tyty);
      return;

    case HIR::Literal::BYTE_STRING:
      translated = compile_byte_string_literal (expr, tyty);
      return;
    }
}

void
CompileExpr::visit (HIR::AssignmentExpr &expr)
{
  auto lvalue = CompileExpr::Compile (expr.get_lhs ().get (), ctx);
  auto rvalue = CompileExpr::Compile (expr.get_rhs ().get (), ctx);

  // assignments are coercion sites so lets convert the rvalue if necessary
  TyTy::BaseType *expected = nullptr;
  TyTy::BaseType *actual = nullptr;

  bool ok;
  ok = ctx->get_tyctx ()->lookup_type (
    expr.get_lhs ()->get_mappings ().get_hirid (), &expected);
  rust_assert (ok);

  ok = ctx->get_tyctx ()->lookup_type (
    expr.get_rhs ()->get_mappings ().get_hirid (), &actual);
  rust_assert (ok);

  rvalue = coercion_site (expr.get_mappings ().get_hirid (), rvalue, actual,
			  expected, expr.get_lhs ()->get_locus (),
			  expr.get_rhs ()->get_locus ());

  // rust_debug_loc (expr.get_locus (), "XXXXXX assignment");
  // debug_tree (rvalue);
  // debug_tree (lvalue);

  tree assignment
    = Backend::assignment_statement (lvalue, rvalue, expr.get_locus ());

  ctx->add_statement (assignment);
}

// Helper for CompileExpr::visit (HIR::MatchExpr).
// Check that the scrutinee of EXPR is a valid kind of expression to match on.
// Return the TypeKind of the scrutinee if it is valid, or TyTy::TypeKind::ERROR
// if not.
static TyTy::TypeKind
check_match_scrutinee (HIR::MatchExpr &expr, Context *ctx)
{
  TyTy::BaseType *scrutinee_expr_tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (
	expr.get_scrutinee_expr ()->get_mappings ().get_hirid (),
	&scrutinee_expr_tyty))
    {
      return TyTy::TypeKind::ERROR;
    }

  TyTy::TypeKind scrutinee_kind = scrutinee_expr_tyty->get_kind ();
  rust_assert ((TyTy::is_primitive_type_kind (scrutinee_kind)
		&& scrutinee_kind != TyTy::TypeKind::NEVER)
	       || scrutinee_kind == TyTy::TypeKind::ADT
	       || scrutinee_kind == TyTy::TypeKind::TUPLE
	       || scrutinee_kind == TyTy::TypeKind::REF);

  if (scrutinee_kind == TyTy::TypeKind::ADT)
    {
      // this will need to change but for now the first pass implementation,
      // lets assert this is the case
      TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (scrutinee_expr_tyty);
      rust_assert (adt->is_enum ());
      rust_assert (adt->number_of_variants () > 0);
    }
  else if (scrutinee_kind == TyTy::TypeKind::FLOAT)
    {
      // FIXME: CASE_LABEL_EXPR does not support floating point types.
      // Find another way to compile these.
      rust_sorry_at (expr.get_locus (),
		     "match on floating-point types is not yet supported");
    }

  TyTy::BaseType *expr_tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &expr_tyty))
    {
      return TyTy::TypeKind::ERROR;
    }

  return scrutinee_kind;
}

void
CompileExpr::visit (HIR::MatchExpr &expr)
{
  // https://gcc.gnu.org/onlinedocs/gccint/Basic-Statements.html#Basic-Statements
  // TODO
  // SWITCH_ALL_CASES_P is true if the switch includes a default label or the
  // case label ranges cover all possible values of the condition expression

  TyTy::TypeKind scrutinee_kind = check_match_scrutinee (expr, ctx);
  if (scrutinee_kind == TyTy::TypeKind::ERROR)
    {
      translated = error_mark_node;
      return;
    }

  TyTy::BaseType *expr_tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &expr_tyty))
    {
      translated = error_mark_node;
      return;
    }

  fncontext fnctx = ctx->peek_fn ();
  Bvariable *tmp = NULL;
  tree enclosing_scope = ctx->peek_enclosing_scope ();
  tree block_type = TyTyResolveCompile::compile (ctx, expr_tyty);

  bool is_address_taken = false;
  tree ret_var_stmt = nullptr;
  tmp = Backend::temporary_variable (fnctx.fndecl, enclosing_scope, block_type,
				     NULL, is_address_taken, expr.get_locus (),
				     &ret_var_stmt);
  ctx->add_statement (ret_var_stmt);

  // lets compile the scrutinee expression
  tree match_scrutinee_rval
    = CompileExpr::Compile (expr.get_scrutinee_expr ().get (), ctx);

  Bvariable *match_scrutinee_tmp_var
    = Backend::temporary_variable (fnctx.fndecl, enclosing_scope,
				   TREE_TYPE (match_scrutinee_rval), NULL,
				   is_address_taken, expr.get_locus (),
				   &ret_var_stmt);
  ctx->add_statement (ret_var_stmt);

  tree match_scrutinee_expr = match_scrutinee_tmp_var->get_tree (
    expr.get_scrutinee_expr ()->get_locus ());

  tree assignment
    = Backend::assignment_statement (match_scrutinee_expr, match_scrutinee_rval,
				     expr.get_locus ());
  ctx->add_statement (assignment);

  // setup the end label so the cases can exit properly
  tree fndecl = fnctx.fndecl;
  location_t end_label_locus = expr.get_locus (); // FIXME
  tree end_label
    = Backend::label (fndecl, "" /* empty creates an artificial label */,
		      end_label_locus);
  tree end_label_decl_statement
    = Backend::label_definition_statement (end_label);

  for (auto &kase : expr.get_match_cases ())
    {
      // for now lets just get single pattern's working
      HIR::MatchArm &kase_arm = kase.get_arm ();
      rust_assert (kase_arm.get_patterns ().size () > 0);

      for (auto &kase_pattern : kase_arm.get_patterns ())
	{
	  // setup the match-arm-body-block
	  location_t start_location = UNKNOWN_LOCATION; // FIXME
	  location_t end_location = UNKNOWN_LOCATION;	// FIXME
	  tree arm_body_block = Backend::block (fndecl, enclosing_scope, {},
						start_location, end_location);

	  ctx->push_block (arm_body_block);

	  // setup the bindings for the block
	  CompilePatternBindings::Compile (kase_pattern.get (),
					   match_scrutinee_expr, ctx);

	  // compile the expr and setup the assignment if required when tmp !=
	  // NULL
	  location_t arm_locus = kase_arm.get_locus ();
	  tree kase_expr_tree
	    = CompileExpr::Compile (kase.get_expr ().get (), ctx);
	  tree result_reference = Backend::var_expression (tmp, arm_locus);
	  tree assignment
	    = Backend::assignment_statement (result_reference, kase_expr_tree,
					     arm_locus);
	  ctx->add_statement (assignment);

	  // go to end label
	  tree goto_end_label
	    = build1_loc (arm_locus, GOTO_EXPR, void_type_node, end_label);
	  ctx->add_statement (goto_end_label);

	  ctx->pop_block ();

	  tree check_expr
	    = CompilePatternCheckExpr::Compile (kase_pattern.get (),
						match_scrutinee_expr, ctx);

	  tree check_stmt
	    = Backend::if_statement (NULL_TREE, check_expr, arm_body_block,
				     NULL_TREE, kase_pattern->get_locus ());

	  ctx->add_statement (check_stmt);
	}
    }

  // setup the switch expression
  ctx->add_statement (end_label_decl_statement);

  translated = Backend::var_expression (tmp, expr.get_locus ());
}

void
CompileExpr::visit (HIR::CallExpr &expr)
{
  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (
	expr.get_fnexpr ()->get_mappings ().get_hirid (), &tyty))
    {
      rust_error_at (expr.get_locus (), "unknown type");
      return;
    }

  // must be a tuple constructor
  bool is_adt_ctor = tyty->get_kind () == TyTy::TypeKind::ADT;
  if (is_adt_ctor)
    {
      rust_assert (tyty->get_kind () == TyTy::TypeKind::ADT);
      TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (tyty);
      tree compiled_adt_type = TyTyResolveCompile::compile (ctx, tyty);

      // what variant is it?
      int union_disriminator = -1;
      TyTy::VariantDef *variant = nullptr;
      if (!adt->is_enum ())
	{
	  rust_assert (adt->number_of_variants () == 1);
	  variant = adt->get_variants ().at (0);
	}
      else
	{
	  HirId variant_id;
	  bool ok = ctx->get_tyctx ()->lookup_variant_definition (
	    expr.get_fnexpr ()->get_mappings ().get_hirid (), &variant_id);
	  rust_assert (ok);

	  ok = adt->lookup_variant_by_id (variant_id, &variant,
					  &union_disriminator);
	  rust_assert (ok);
	}

      // this assumes all fields are in order from type resolution and if a
      // base struct was specified those fields are filed via accessors
      std::vector<tree> arguments;
      for (size_t i = 0; i < expr.get_arguments ().size (); i++)
	{
	  auto &argument = expr.get_arguments ().at (i);
	  auto rvalue = CompileExpr::Compile (argument.get (), ctx);

	  // assignments are coercion sites so lets convert the rvalue if
	  // necessary
	  auto respective_field = variant->get_field_at_index (i);
	  auto expected = respective_field->get_field_type ();

	  TyTy::BaseType *actual = nullptr;
	  bool ok = ctx->get_tyctx ()->lookup_type (
	    argument->get_mappings ().get_hirid (), &actual);
	  rust_assert (ok);

	  // coerce it if required
	  location_t lvalue_locus
	    = ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
	  location_t rvalue_locus = argument->get_locus ();
	  rvalue
	    = coercion_site (argument->get_mappings ().get_hirid (), rvalue,
			     actual, expected, lvalue_locus, rvalue_locus);

	  // add it to the list
	  arguments.push_back (rvalue);
	}

      // the constructor depends on whether this is actually an enum or not if
      // its an enum we need to setup the discriminator
      std::vector<tree> ctor_arguments;
      if (adt->is_enum ())
	{
	  HIR::Expr *discrim_expr = variant->get_discriminant ();
	  tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);
	  tree folded_discrim_expr = fold_expr (discrim_expr_node);
	  tree qualifier = folded_discrim_expr;

	  ctor_arguments.push_back (qualifier);
	}
      for (auto &arg : arguments)
	ctor_arguments.push_back (arg);

      translated
	= Backend::constructor_expression (compiled_adt_type, adt->is_enum (),
					   ctor_arguments, union_disriminator,
					   expr.get_locus ());

      return;
    }

  auto get_parameter_tyty_at_index
    = [] (const TyTy::BaseType *base, size_t index,
	  TyTy::BaseType **result) -> bool {
    bool is_fn = base->get_kind () == TyTy::TypeKind::FNDEF
		 || base->get_kind () == TyTy::TypeKind::FNPTR;
    rust_assert (is_fn);

    if (base->get_kind () == TyTy::TypeKind::FNPTR)
      {
	const TyTy::FnPtr *fn = static_cast<const TyTy::FnPtr *> (base);
	*result = fn->get_param_type_at (index);

	return true;
      }

    const TyTy::FnType *fn = static_cast<const TyTy::FnType *> (base);
    auto param = fn->param_at (index);
    *result = param.second;

    return true;
  };

  auto fn_address = CompileExpr::Compile (expr.get_fnexpr ().get (), ctx);

  // is this a closure call?
  bool possible_trait_call
    = generate_possible_fn_trait_call (expr, fn_address, &translated);
  if (possible_trait_call)
    return;

  bool is_variadic = false;
  size_t required_num_args = expr.get_arguments ().size ();

  if (tyty->get_kind () == TyTy::TypeKind::FNDEF)
    {
      const TyTy::FnType *fn = static_cast<const TyTy::FnType *> (tyty);
      required_num_args = fn->num_params ();
      is_variadic = fn->is_variadic ();
    }
  else if (tyty->get_kind () == TyTy::TypeKind::FNPTR)
    {
      const TyTy::FnPtr *fn = static_cast<const TyTy::FnPtr *> (tyty);
      required_num_args = fn->num_params ();
    }

  std::vector<tree> args;
  for (size_t i = 0; i < expr.get_arguments ().size (); i++)
    {
      auto &argument = expr.get_arguments ().at (i);
      auto rvalue = CompileExpr::Compile (argument.get (), ctx);

      if (is_variadic && i >= required_num_args)
	{
	  args.push_back (rvalue);
	  continue;
	}

      // assignments are coercion sites so lets convert the rvalue if
      // necessary
      bool ok;
      TyTy::BaseType *expected = nullptr;
      ok = get_parameter_tyty_at_index (tyty, i, &expected);
      rust_assert (ok);

      TyTy::BaseType *actual = nullptr;
      ok = ctx->get_tyctx ()->lookup_type (
	argument->get_mappings ().get_hirid (), &actual);
      rust_assert (ok);

      // coerce it if required
      location_t lvalue_locus
	= ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
      location_t rvalue_locus = argument->get_locus ();
      rvalue = coercion_site (argument->get_mappings ().get_hirid (), rvalue,
			      actual, expected, lvalue_locus, rvalue_locus);

      // add it to the list
      args.push_back (rvalue);
    }

  // must be a regular call to a function
  translated
    = Backend::call_expression (fn_address, args, nullptr, expr.get_locus ());
}

void
CompileExpr::visit (HIR::MethodCallExpr &expr)
{
  // method receiver
  tree self = CompileExpr::Compile (expr.get_receiver ().get (), ctx);

  // lookup the expected function type
  TyTy::BaseType *lookup_fntype = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    expr.get_method_name ().get_mappings ().get_hirid (), &lookup_fntype);
  rust_assert (ok);
  rust_assert (lookup_fntype->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *fntype = static_cast<TyTy::FnType *> (lookup_fntype);

  TyTy::BaseType *receiver = nullptr;
  ok = ctx->get_tyctx ()->lookup_receiver (expr.get_mappings ().get_hirid (),
					   &receiver);
  rust_assert (ok);

  bool is_dyn_dispatch
    = receiver->get_root ()->get_kind () == TyTy::TypeKind::DYNAMIC;
  bool is_generic_receiver = receiver->get_kind () == TyTy::TypeKind::PARAM;
  if (is_generic_receiver)
    {
      TyTy::ParamType *p = static_cast<TyTy::ParamType *> (receiver);
      receiver = p->resolve ();
    }

  tree fn_expr = error_mark_node;
  if (is_dyn_dispatch)
    {
      const TyTy::DynamicObjectType *dyn
	= static_cast<const TyTy::DynamicObjectType *> (receiver->get_root ());

      std::vector<HIR::Expr *> arguments;
      for (auto &arg : expr.get_arguments ())
	arguments.push_back (arg.get ());

      fn_expr
	= get_fn_addr_from_dyn (dyn, receiver, fntype, self, expr.get_locus ());
      self = get_receiver_from_dyn (dyn, receiver, fntype, self,
				    expr.get_locus ());
    }
  else
    {
      // lookup compiled functions since it may have already been compiled
      HIR::PathExprSegment method_name = expr.get_method_name ();
      HIR::PathIdentSegment segment_name = method_name.get_segment ();
      fn_expr = resolve_method_address (fntype, receiver, expr.get_locus ());
    }

  // lookup the autoderef mappings
  HirId autoderef_mappings_id
    = expr.get_receiver ()->get_mappings ().get_hirid ();
  std::vector<Resolver::Adjustment> *adjustments = nullptr;
  ok = ctx->get_tyctx ()->lookup_autoderef_mappings (autoderef_mappings_id,
						     &adjustments);
  rust_assert (ok);

  // apply adjustments for the fn call
  self = resolve_adjustements (*adjustments, self,
			       expr.get_receiver ()->get_locus ());

  std::vector<tree> args;
  args.push_back (self); // adjusted self

  // normal args
  for (size_t i = 0; i < expr.get_arguments ().size (); i++)
    {
      auto &argument = expr.get_arguments ().at (i);
      auto rvalue = CompileExpr::Compile (argument.get (), ctx);

      // assignments are coercion sites so lets convert the rvalue if
      // necessary, offset from the already adjusted implicit self
      bool ok;
      TyTy::BaseType *expected = fntype->param_at (i + 1).second;

      TyTy::BaseType *actual = nullptr;
      ok = ctx->get_tyctx ()->lookup_type (
	argument->get_mappings ().get_hirid (), &actual);
      rust_assert (ok);

      // coerce it if required
      location_t lvalue_locus
	= ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
      location_t rvalue_locus = argument->get_locus ();
      rvalue = coercion_site (argument->get_mappings ().get_hirid (), rvalue,
			      actual, expected, lvalue_locus, rvalue_locus);

      // add it to the list
      args.push_back (rvalue);
    }

  translated
    = Backend::call_expression (fn_expr, args, nullptr, expr.get_locus ());
}

tree
CompileExpr::get_fn_addr_from_dyn (const TyTy::DynamicObjectType *dyn,
				   TyTy::BaseType *receiver,
				   TyTy::FnType *fntype, tree receiver_ref,
				   location_t expr_locus)
{
  size_t offs = 0;
  const Resolver::TraitItemReference *ref = nullptr;
  for (auto &bound : dyn->get_object_items ())
    {
      const Resolver::TraitItemReference *item = bound.first;
      auto t = item->get_tyty ();
      rust_assert (t->get_kind () == TyTy::TypeKind::FNDEF);
      auto ft = static_cast<TyTy::FnType *> (t);

      if (ft->get_id () == fntype->get_id ())
	{
	  ref = item;
	  break;
	}
      offs++;
    }

  if (ref == nullptr)
    return error_mark_node;

  // cast it to the correct fntype
  tree expected_fntype = TyTyResolveCompile::compile (ctx, fntype, true);
  tree idx = build_int_cst (size_type_node, offs);

  tree vtable_ptr
    = Backend::struct_field_expression (receiver_ref, 1, expr_locus);
  tree vtable_array_access
    = build4_loc (expr_locus, ARRAY_REF, TREE_TYPE (TREE_TYPE (vtable_ptr)),
		  vtable_ptr, idx, NULL_TREE, NULL_TREE);

  tree vcall = build3_loc (expr_locus, OBJ_TYPE_REF, expected_fntype,
			   vtable_array_access, receiver_ref, idx);

  return vcall;
}

tree
CompileExpr::get_receiver_from_dyn (const TyTy::DynamicObjectType *dyn,
				    TyTy::BaseType *receiver,
				    TyTy::FnType *fntype, tree receiver_ref,
				    location_t expr_locus)
{
  // access the offs + 1 for the fnptr and offs=0 for the reciever obj
  return Backend::struct_field_expression (receiver_ref, 0, expr_locus);
}

tree
CompileExpr::resolve_operator_overload (LangItem::Kind lang_item_type,
					HIR::OperatorExprMeta expr, tree lhs,
					tree rhs, HIR::Expr *lhs_expr,
					HIR::Expr *rhs_expr)
{
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  rust_assert (is_op_overload);

  TyTy::BaseType *receiver = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_receiver (expr.get_mappings ().get_hirid (),
					  &receiver);
  rust_assert (ok);

  bool is_generic_receiver = receiver->get_kind () == TyTy::TypeKind::PARAM;
  if (is_generic_receiver)
    {
      TyTy::ParamType *p = static_cast<TyTy::ParamType *> (receiver);
      receiver = p->resolve ();
    }

  // lookup compiled functions since it may have already been compiled
  HIR::PathIdentSegment segment_name (LangItem::ToString (lang_item_type));
  tree fn_expr = resolve_method_address (fntype, receiver, expr.get_locus ());

  // lookup the autoderef mappings
  std::vector<Resolver::Adjustment> *adjustments = nullptr;
  ok = ctx->get_tyctx ()->lookup_autoderef_mappings (
    expr.get_lvalue_mappings ().get_hirid (), &adjustments);
  rust_assert (ok);

  // apply adjustments for the fn call
  tree self = resolve_adjustements (*adjustments, lhs, lhs_expr->get_locus ());

  std::vector<tree> args;
  args.push_back (self); // adjusted self
  if (rhs != nullptr)	 // can be null for negation_expr (unary ones)
    args.push_back (rhs);

  return Backend::call_expression (fn_expr, args, nullptr, expr.get_locus ());
}

tree
CompileExpr::compile_bool_literal (const HIR::LiteralExpr &expr,
				   const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::BOOL);

  const auto literal_value = expr.get_literal ();
  bool bval = literal_value.as_string ().compare ("true") == 0;
  return Backend::boolean_constant_expression (bval);
}

tree
CompileExpr::compile_integer_literal (const HIR::LiteralExpr &expr,
				      const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::INT);
  const auto literal_value = expr.get_literal ();

  tree type = TyTyResolveCompile::compile (ctx, tyty);

  mpz_t ival;
  if (mpz_init_set_str (ival, literal_value.as_string ().c_str (), 10) != 0)
    {
      rust_error_at (expr.get_locus (), "bad number in literal");
      return error_mark_node;
    }

  mpz_t type_min;
  mpz_t type_max;
  mpz_init (type_min);
  mpz_init (type_max);
  get_type_static_bounds (type, type_min, type_max);

  if (mpz_cmp (ival, type_min) < 0 || mpz_cmp (ival, type_max) > 0)
    {
      rust_error_at (expr.get_locus (),
		     "integer overflows the respective type %qs",
		     tyty->get_name ().c_str ());
      return error_mark_node;
    }

  tree result = wide_int_to_tree (type, wi::from_mpz (type, ival, true));

  mpz_clear (type_min);
  mpz_clear (type_max);
  mpz_clear (ival);

  return result;
}

tree
CompileExpr::compile_float_literal (const HIR::LiteralExpr &expr,
				    const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::FLOAT);
  const auto literal_value = expr.get_literal ();

  mpfr_t fval;
  if (mpfr_init_set_str (fval, literal_value.as_string ().c_str (), 10,
			 MPFR_RNDN)
      != 0)
    {
      rust_error_at (expr.get_locus (), "bad number in literal");
      return error_mark_node;
    }

  tree type = TyTyResolveCompile::compile (ctx, tyty);

  // taken from:
  // see go/gofrontend/expressions.cc:check_float_type
  mpfr_exp_t exp = mpfr_get_exp (fval);
  bool real_value_overflow = exp > TYPE_PRECISION (type);

  REAL_VALUE_TYPE r1;
  real_from_mpfr (&r1, fval, type, GMP_RNDN);
  REAL_VALUE_TYPE r2;
  real_convert (&r2, TYPE_MODE (type), &r1);

  tree real_value = build_real (type, r2);
  if (TREE_OVERFLOW (real_value) || real_value_overflow)
    {
      rust_error_at (expr.get_locus (),
		     "decimal overflows the respective type %qs",
		     tyty->get_name ().c_str ());
      return error_mark_node;
    }

  return real_value;
}

tree
CompileExpr::compile_char_literal (const HIR::LiteralExpr &expr,
				   const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::CHAR);
  const auto literal_value = expr.get_literal ();

  // FIXME needs wchar_t
  char c = literal_value.as_string ().c_str ()[0];
  return Backend::wchar_constant_expression (c);
}

tree
CompileExpr::compile_byte_literal (const HIR::LiteralExpr &expr,
				   const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::BYTE);
  const auto literal_value = expr.get_literal ();

  tree type = TyTyResolveCompile::compile (ctx, tyty);
  char c = literal_value.as_string ().c_str ()[0];
  return build_int_cst (type, c);
}

tree
CompileExpr::compile_string_literal (const HIR::LiteralExpr &expr,
				     const TyTy::BaseType *tyty)
{
  tree fat_pointer = TyTyResolveCompile::compile (ctx, tyty);

  rust_assert (expr.get_lit_type () == HIR::Literal::STRING);
  const auto literal_value = expr.get_literal ();

  auto base = Backend::string_constant_expression (literal_value.as_string ());
  tree data = address_expression (base, expr.get_locus ());

  TyTy::BaseType *usize = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_builtin ("usize", &usize);
  rust_assert (ok);
  tree type = TyTyResolveCompile::compile (ctx, usize);

  tree size = build_int_cstu (type, literal_value.as_string ().size ());

  return Backend::constructor_expression (fat_pointer, false, {data, size}, -1,
					  expr.get_locus ());
}

tree
CompileExpr::compile_byte_string_literal (const HIR::LiteralExpr &expr,
					  const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::BYTE_STRING);

  // the type here is &[ty; capacity]
  rust_assert (tyty->get_kind () == TyTy::TypeKind::REF);
  const auto ref_tyty = static_cast<const TyTy::ReferenceType *> (tyty);
  auto base_tyty = ref_tyty->get_base ();
  rust_assert (base_tyty->get_kind () == TyTy::TypeKind::ARRAY);
  auto array_tyty = static_cast<TyTy::ArrayType *> (base_tyty);

  std::string value_str = expr.get_literal ().as_string ();
  std::vector<tree> vals;
  std::vector<unsigned long> indexes;
  for (size_t i = 0; i < value_str.size (); i++)
    {
      char b = value_str.at (i);
      tree bb = Backend::char_constant_expression (b);
      vals.push_back (bb);
      indexes.push_back (i);
    }

  tree array_type = TyTyResolveCompile::compile (ctx, array_tyty);
  tree constructed
    = Backend::array_constructor_expression (array_type, indexes, vals,
					     expr.get_locus ());

  return address_expression (constructed, expr.get_locus ());
}

tree
CompileExpr::type_cast_expression (tree type_to_cast_to, tree expr_tree,
				   location_t location)
{
  if (type_to_cast_to == error_mark_node || expr_tree == error_mark_node
      || TREE_TYPE (expr_tree) == error_mark_node)
    return error_mark_node;

  if (Backend::type_size (type_to_cast_to) == 0
      || TREE_TYPE (expr_tree) == void_type_node)
    {
      // Do not convert zero-sized types.
      return expr_tree;
    }
  else if (TREE_CODE (type_to_cast_to) == INTEGER_TYPE)
    {
      tree cast = convert_to_integer (type_to_cast_to, expr_tree);
      // FIXME check for TREE_OVERFLOW?
      return cast;
    }
  else if (TREE_CODE (type_to_cast_to) == REAL_TYPE)
    {
      tree cast = convert_to_real (type_to_cast_to, expr_tree);
      // FIXME
      // We might need to check that the tree is MAX val and thusly saturate it
      // to inf. we can get the bounds and check the value if its >= or <= to
      // the min and max bounds
      //
      // https://github.com/Rust-GCC/gccrs/issues/635
      return cast;
    }
  else if (TREE_CODE (type_to_cast_to) == COMPLEX_TYPE)
    {
      return convert_to_complex (type_to_cast_to, expr_tree);
    }
  else if (TREE_CODE (type_to_cast_to) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (expr_tree)) == INTEGER_TYPE)
    {
      return convert_to_pointer (type_to_cast_to, expr_tree);
    }
  else if (TREE_CODE (type_to_cast_to) == RECORD_TYPE
	   || TREE_CODE (type_to_cast_to) == ARRAY_TYPE)
    {
      return fold_build1_loc (location, VIEW_CONVERT_EXPR, type_to_cast_to,
			      expr_tree);
    }
  else if (TREE_CODE (type_to_cast_to) == POINTER_TYPE
	   && RS_DST_FLAG (TREE_TYPE (expr_tree)))
    {
      // returning a raw cast using NOP_EXPR seems to resut in an ICE:
      //
      // Analyzing compilation unit
      // Performing interprocedural optimizations
      //  <*free_lang_data> {heap 2644k} <visibility> {heap 2644k}
      //  <build_ssa_passes> {heap 2644k} <opt_local_passes> {heap 2644k}during
      //  GIMPLE pass: cddce
      // In function ‘*T::as_ptr<i32>’:
      // rust1: internal compiler error: in propagate_necessity, at
      // tree-ssa-dce.cc:984 0x1d5b43e propagate_necessity
      //         ../../gccrs/gcc/tree-ssa-dce.cc:984
      // 0x1d5e180 perform_tree_ssa_dce
      //         ../../gccrs/gcc/tree-ssa-dce.cc:1876
      // 0x1d5e2c8 tree_ssa_cd_dce
      //         ../../gccrs/gcc/tree-ssa-dce.cc:1920
      // 0x1d5e49a execute
      //         ../../gccrs/gcc/tree-ssa-dce.cc:1992

      // this is returning the direct raw pointer of the slice an assumes a very
      // specific layout
      return Backend::struct_field_expression (expr_tree, 0, location);
    }

  return fold_convert_loc (location, type_to_cast_to, expr_tree);
}

void
CompileExpr::visit (HIR::ArrayExpr &expr)
{
  TyTy::BaseType *tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &tyty))
    {
      rust_fatal_error (expr.get_locus (),
			"did not resolve type for this array expr");
      return;
    }

  tree array_type = TyTyResolveCompile::compile (ctx, tyty);
  if (TREE_CODE (array_type) != ARRAY_TYPE)
    {
      translated = error_mark_node;
      return;
    }

  rust_assert (tyty->get_kind () == TyTy::TypeKind::ARRAY);
  const TyTy::ArrayType &array_tyty
    = static_cast<const TyTy::ArrayType &> (*tyty);

  HIR::ArrayElems &elements = *expr.get_internal_elements ();
  switch (elements.get_array_expr_type ())
    {
      case HIR::ArrayElems::ArrayExprType::VALUES: {
	HIR::ArrayElemsValues &elems
	  = static_cast<HIR::ArrayElemsValues &> (elements);
	translated
	  = array_value_expr (expr.get_locus (), array_tyty, array_type, elems);
      }
      return;

    case HIR::ArrayElems::ArrayExprType::COPIED:
      HIR::ArrayElemsCopied &elems
	= static_cast<HIR::ArrayElemsCopied &> (elements);
      translated
	= array_copied_expr (expr.get_locus (), array_tyty, array_type, elems);
    }
}

tree
CompileExpr::array_value_expr (location_t expr_locus,
			       const TyTy::ArrayType &array_tyty,
			       tree array_type, HIR::ArrayElemsValues &elems)
{
  std::vector<unsigned long> indexes;
  std::vector<tree> constructor;
  size_t i = 0;
  for (auto &elem : elems.get_values ())
    {
      tree translated_expr = CompileExpr::Compile (elem.get (), ctx);
      constructor.push_back (translated_expr);
      indexes.push_back (i++);
    }

  return Backend::array_constructor_expression (array_type, indexes,
						constructor, expr_locus);
}

tree
CompileExpr::array_copied_expr (location_t expr_locus,
				const TyTy::ArrayType &array_tyty,
				tree array_type, HIR::ArrayElemsCopied &elems)
{
  //  see gcc/cp/typeck2.cc:1369-1401
  gcc_assert (TREE_CODE (array_type) == ARRAY_TYPE);
  tree domain = TYPE_DOMAIN (array_type);
  if (!domain)
    return error_mark_node;

  if (!TREE_CONSTANT (TYPE_MAX_VALUE (domain)))
    {
      rust_error_at (expr_locus, "non const capacity domain %qT", array_type);
      return error_mark_node;
    }

  ctx->push_const_context ();
  tree capacity_expr
    = CompileExpr::Compile (elems.get_num_copies_expr ().get (), ctx);
  ctx->pop_const_context ();

  if (!TREE_CONSTANT (capacity_expr))
    {
      rust_error_at (expr_locus, "non const num copies %qT", array_type);
      return error_mark_node;
    }

  // get the compiled value
  tree translated_expr
    = CompileExpr::Compile (elems.get_elem_to_copy ().get (), ctx);

  tree max_domain = TYPE_MAX_VALUE (domain);
  tree min_domain = TYPE_MIN_VALUE (domain);

  auto max = wi::to_offset (max_domain);
  auto min = wi::to_offset (min_domain);
  auto precision = TYPE_PRECISION (TREE_TYPE (domain));
  auto sign = TYPE_SIGN (TREE_TYPE (domain));
  unsigned HOST_WIDE_INT len
    = wi::ext (max - min + 1, precision, sign).to_uhwi ();

  // In a const context we must initialize the entire array, which entails
  // allocating for each element. If the user wants a huge array, we will OOM
  // and die horribly.
  if (ctx->const_context_p ())
    {
      size_t idx = 0;
      std::vector<unsigned long> indexes;
      std::vector<tree> constructor;
      for (unsigned HOST_WIDE_INT i = 0; i < len; i++)
	{
	  constructor.push_back (translated_expr);
	  indexes.push_back (idx++);
	}

      return Backend::array_constructor_expression (array_type, indexes,
						    constructor, expr_locus);
    }

  else
    {
      // Create a new block scope in which to initialize the array
      tree fndecl = NULL_TREE;
      if (ctx->in_fn ())
	fndecl = ctx->peek_fn ().fndecl;

      std::vector<Bvariable *> locals;
      tree enclosing_scope = ctx->peek_enclosing_scope ();
      tree init_block = Backend::block (fndecl, enclosing_scope, locals,
					expr_locus, expr_locus);
      ctx->push_block (init_block);

      tree tmp;
      tree stmts = Backend::array_initializer (fndecl, init_block, array_type,
					       capacity_expr, translated_expr,
					       &tmp, expr_locus);
      ctx->add_statement (stmts);

      tree block = ctx->pop_block ();

      // The result is a compound expression which creates a temporary array,
      // initializes all the elements in a loop, and then yeilds the array.
      return Backend::compound_expression (block, tmp, expr_locus);
    }
}

tree
HIRCompileBase::resolve_adjustements (
  std::vector<Resolver::Adjustment> &adjustments, tree expression,
  location_t locus)
{
  tree e = expression;
  for (auto &adjustment : adjustments)
    {
      switch (adjustment.get_type ())
	{
	case Resolver::Adjustment::AdjustmentType::ERROR:
	  return error_mark_node;

	case Resolver::Adjustment::AdjustmentType::IMM_REF:
	  case Resolver::Adjustment::AdjustmentType::MUT_REF: {
	    if (!RS_DST_FLAG (TREE_TYPE (e)))
	      {
		e = address_expression (e, locus);
	      }
	  }
	  break;

	case Resolver::Adjustment::AdjustmentType::DEREF:
	case Resolver::Adjustment::AdjustmentType::DEREF_MUT:
	  e = resolve_deref_adjustment (adjustment, e, locus);
	  break;

	case Resolver::Adjustment::AdjustmentType::INDIRECTION:
	  e = resolve_indirection_adjustment (adjustment, e, locus);
	  break;

	case Resolver::Adjustment::AdjustmentType::UNSIZE:
	  e = resolve_unsized_adjustment (adjustment, e, locus);
	  break;
	}
    }

  return e;
}

tree
HIRCompileBase::resolve_deref_adjustment (Resolver::Adjustment &adjustment,
					  tree expression, location_t locus)
{
  rust_assert (adjustment.is_deref_adjustment ()
	       || adjustment.is_deref_mut_adjustment ());
  rust_assert (adjustment.has_operator_overload ());

  TyTy::FnType *lookup = adjustment.get_deref_operator_fn ();
  TyTy::BaseType *receiver = adjustment.get_actual ();
  tree fn_address = resolve_method_address (lookup, receiver, locus);

  // does it need a reference to call
  tree adjusted_argument = expression;
  bool needs_borrow = adjustment.get_deref_adjustment_type ()
		      != Resolver::Adjustment::AdjustmentType::ERROR;
  if (needs_borrow)
    {
      adjusted_argument = address_expression (expression, locus);
    }

  // make the call
  return Backend::call_expression (fn_address, {adjusted_argument}, nullptr,
				   locus);
}

tree
HIRCompileBase::resolve_indirection_adjustment (
  Resolver::Adjustment &adjustment, tree expression, location_t locus)
{
  return indirect_expression (expression, locus);
}

tree
HIRCompileBase::resolve_unsized_adjustment (Resolver::Adjustment &adjustment,
					    tree expression, location_t locus)
{
  bool expect_slice
    = adjustment.get_expected ()->get_kind () == TyTy::TypeKind::SLICE;
  bool expect_dyn
    = adjustment.get_expected ()->get_kind () == TyTy::TypeKind::DYNAMIC;

  // assumes this is an array
  tree expr_type = TREE_TYPE (expression);
  if (expect_slice)
    {
      rust_assert (TREE_CODE (expr_type) == ARRAY_TYPE);
      return resolve_unsized_slice_adjustment (adjustment, expression, locus);
    }

  rust_assert (expect_dyn);
  return resolve_unsized_dyn_adjustment (adjustment, expression, locus);
}

tree
HIRCompileBase::resolve_unsized_slice_adjustment (
  Resolver::Adjustment &adjustment, tree expression, location_t locus)
{
  // assumes this is an array
  tree expr_type = TREE_TYPE (expression);
  rust_assert (TREE_CODE (expr_type) == ARRAY_TYPE);

  // takes an array and returns a fat-pointer so this becomes a constructor
  // expression
  rust_assert (adjustment.get_expected ()->get_kind ()
	       == TyTy::TypeKind::SLICE);
  tree fat_pointer
    = TyTyResolveCompile::compile (ctx, adjustment.get_expected ());

  // make a constructor for this
  tree data = address_expression (expression, locus);

  // fetch the size from the domain
  tree domain = TYPE_DOMAIN (expr_type);
  unsigned HOST_WIDE_INT array_size
    = wi::ext (wi::to_offset (TYPE_MAX_VALUE (domain))
		 - wi::to_offset (TYPE_MIN_VALUE (domain)) + 1,
	       TYPE_PRECISION (TREE_TYPE (domain)),
	       TYPE_SIGN (TREE_TYPE (domain)))
	.to_uhwi ();
  tree size = build_int_cstu (size_type_node, array_size);

  return Backend::constructor_expression (fat_pointer, false, {data, size}, -1,
					  locus);
}

tree
HIRCompileBase::resolve_unsized_dyn_adjustment (
  Resolver::Adjustment &adjustment, tree expression, location_t locus)
{
  tree rvalue = expression;
  location_t rvalue_locus = locus;

  const TyTy::BaseType *actual = adjustment.get_actual ();
  const TyTy::BaseType *expected = adjustment.get_expected ();

  const TyTy::DynamicObjectType *dyn
    = static_cast<const TyTy::DynamicObjectType *> (expected);

  rust_debug ("resolve_unsized_dyn_adjustment actual={%s} dyn={%s}",
	      actual->debug_str ().c_str (), dyn->debug_str ().c_str ());

  return coerce_to_dyn_object (rvalue, actual, dyn, rvalue_locus);
}

void
CompileExpr::visit (HIR::RangeFromToExpr &expr)
{
  tree from = CompileExpr::Compile (expr.get_from_expr ().get (), ctx);
  tree to = CompileExpr::Compile (expr.get_to_expr ().get (), ctx);
  if (from == error_mark_node || to == error_mark_node)
    {
      translated = error_mark_node;
      return;
    }

  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);

  // make the constructor
  translated = Backend::constructor_expression (adt, false, {from, to}, -1,
						expr.get_locus ());
}

void
CompileExpr::visit (HIR::RangeFromExpr &expr)
{
  tree from = CompileExpr::Compile (expr.get_from_expr ().get (), ctx);
  if (from == error_mark_node)
    {
      translated = error_mark_node;
      return;
    }

  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);

  // make the constructor
  translated = Backend::constructor_expression (adt, false, {from}, -1,
						expr.get_locus ());
}

void
CompileExpr::visit (HIR::RangeToExpr &expr)
{
  tree to = CompileExpr::Compile (expr.get_to_expr ().get (), ctx);
  if (to == error_mark_node)
    {
      translated = error_mark_node;
      return;
    }

  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);

  // make the constructor
  translated
    = Backend::constructor_expression (adt, false, {to}, -1, expr.get_locus ());
}

void
CompileExpr::visit (HIR::RangeFullExpr &expr)
{
  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);
  translated
    = Backend::constructor_expression (adt, false, {}, -1, expr.get_locus ());
}

void
CompileExpr::visit (HIR::RangeFromToInclExpr &expr)
{
  tree from = CompileExpr::Compile (expr.get_from_expr ().get (), ctx);
  tree to = CompileExpr::Compile (expr.get_to_expr ().get (), ctx);
  if (from == error_mark_node || to == error_mark_node)
    {
      translated = error_mark_node;
      return;
    }

  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);

  // make the constructor
  translated = Backend::constructor_expression (adt, false, {from, to}, -1,
						expr.get_locus ());
}

void
CompileExpr::visit (HIR::ArrayIndexExpr &expr)
{
  tree array_reference
    = CompileExpr::Compile (expr.get_array_expr ().get (), ctx);
  tree index = CompileExpr::Compile (expr.get_index_expr ().get (), ctx);

  // this might be an core::ops::index lang item situation
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type = LangItem::Kind::INDEX;
      tree operator_overload_call
	= resolve_operator_overload (lang_item_type, expr, array_reference,
				     index, expr.get_array_expr ().get (),
				     expr.get_index_expr ().get ());

      tree actual_type = TREE_TYPE (operator_overload_call);
      bool can_indirect = TYPE_PTR_P (actual_type) || TYPE_REF_P (actual_type);
      if (!can_indirect)
	{
	  // nothing to do
	  translated = operator_overload_call;
	  return;
	}

      // rust deref always returns a reference from this overload then we can
      // actually do the indirection
      translated
	= indirect_expression (operator_overload_call, expr.get_locus ());
      return;
    }

  // lets check if the array is a reference type then we can add an
  // indirection if required
  TyTy::BaseType *array_expr_ty = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    expr.get_array_expr ()->get_mappings ().get_hirid (), &array_expr_ty);
  rust_assert (ok);

  // do we need to add an indirect reference
  if (array_expr_ty->get_kind () == TyTy::TypeKind::REF)
    {
      array_reference
	= indirect_expression (array_reference, expr.get_locus ());
    }

  translated = Backend::array_index_expression (array_reference, index,
						expr.get_locus ());
}

void
CompileExpr::visit (HIR::ClosureExpr &expr)
{
  TyTy::BaseType *closure_expr_ty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &closure_expr_ty))
    {
      rust_fatal_error (expr.get_locus (),
			"did not resolve type for this ClosureExpr");
      return;
    }
  rust_assert (closure_expr_ty->get_kind () == TyTy::TypeKind::CLOSURE);
  TyTy::ClosureType *closure_tyty
    = static_cast<TyTy::ClosureType *> (closure_expr_ty);
  tree compiled_closure_tyty = TyTyResolveCompile::compile (ctx, closure_tyty);

  // generate closure function
  generate_closure_function (expr, *closure_tyty, compiled_closure_tyty);

  // lets ignore state capture for now we need to instantiate the struct anyway
  // then generate the function
  std::vector<tree> vals;
  for (const auto &capture : closure_tyty->get_captures ())
    {
      // lookup the HirId
      HirId ref = UNKNOWN_HIRID;
      bool ok = ctx->get_mappings ()->lookup_node_to_hir (capture, &ref);
      rust_assert (ok);

      // lookup the var decl
      Bvariable *var = nullptr;
      bool found = ctx->lookup_var_decl (ref, &var);
      rust_assert (found);

      // FIXME
      // this should bes based on the closure move-ability
      tree var_expr = var->get_tree (expr.get_locus ());
      tree val = address_expression (var_expr, expr.get_locus ());
      vals.push_back (val);
    }

  translated = Backend::constructor_expression (compiled_closure_tyty, false,
						vals, -1, expr.get_locus ());
}

tree
CompileExpr::generate_closure_function (HIR::ClosureExpr &expr,
					TyTy::ClosureType &closure_tyty,
					tree compiled_closure_tyty)
{
  TyTy::FnType *fn_tyty = nullptr;
  tree compiled_fn_type
    = generate_closure_fntype (expr, closure_tyty, compiled_closure_tyty,
			       &fn_tyty);
  if (compiled_fn_type == error_mark_node)
    return error_mark_node;

  const Resolver::CanonicalPath &parent_canonical_path
    = closure_tyty.get_ident ().path;
  NodeId node_id;
  bool ok = ctx->get_mappings ()->lookup_hir_to_node (
    expr.get_mappings ().get_hirid (), &node_id);
  rust_assert (ok);
  Resolver::CanonicalPath path = parent_canonical_path.append (
    Resolver::CanonicalPath::new_seg (node_id, "{{closure}}"));

  std::string ir_symbol_name = path.get ();
  std::string asm_name = ctx->mangle_item (&closure_tyty, path);

  unsigned int flags = 0;
  tree fndecl = Backend::function (compiled_fn_type, ir_symbol_name, asm_name,
				   flags, expr.get_locus ());

  // insert into the context
  ctx->insert_function_decl (fn_tyty, fndecl);
  ctx->insert_closure_decl (&closure_tyty, fndecl);

  // setup the parameters
  std::vector<Bvariable *> param_vars;

  // closure self
  Bvariable *self_param
    = Backend::parameter_variable (fndecl, "$closure", compiled_closure_tyty,
				   expr.get_locus ());
  DECL_ARTIFICIAL (self_param->get_decl ()) = 1;
  param_vars.push_back (self_param);

  // push a new context
  ctx->push_closure_context (expr.get_mappings ().get_hirid ());

  // setup the implicit argument captures
  size_t idx = 0;
  for (const auto &capture : closure_tyty.get_captures ())
    {
      // lookup the HirId
      HirId ref = UNKNOWN_HIRID;
      bool ok = ctx->get_mappings ()->lookup_node_to_hir (capture, &ref);
      rust_assert (ok);

      // get the assessor
      tree binding = Backend::struct_field_expression (self_param->get_tree (
							 expr.get_locus ()),
						       idx, expr.get_locus ());
      tree indirection = indirect_expression (binding, expr.get_locus ());

      // insert bindings
      ctx->insert_closure_binding (ref, indirection);

      // continue
      idx++;
    }

  // args tuple
  tree args_type
    = TyTyResolveCompile::compile (ctx, &closure_tyty.get_parameters ());
  Bvariable *args_param
    = Backend::parameter_variable (fndecl, "args", args_type,
				   expr.get_locus ());
  param_vars.push_back (args_param);

  // setup the implicit mappings for the arguments. Since argument passing to
  // closure functions is done via passing a tuple but the closure body expects
  // just normal arguments this means we need to destructure them similar to
  // what we do in MatchExpr's. This means when we have a closure-param of a we
  // actually setup the destructure to take from the args tuple

  tree args_param_expr = args_param->get_tree (expr.get_locus ());
  size_t i = 0;
  for (auto &closure_param : expr.get_params ())
    {
      tree compiled_param_var
	= Backend::struct_field_expression (args_param_expr, i,
					    closure_param.get_locus ());

      CompilePatternBindings::Compile (closure_param.get_pattern ().get (),
				       compiled_param_var, ctx);
      i++;
    }

  if (!Backend::function_set_parameters (fndecl, param_vars))
    {
      ctx->pop_closure_context ();
      return error_mark_node;
    }

  // lookup locals
  HIR::Expr *function_body = expr.get_expr ().get ();
  bool is_block_expr
    = function_body->get_expression_type () == HIR::Expr::ExprType::Block;

  if (is_block_expr)
    {
      auto body_mappings = function_body->get_mappings ();
      if (flag_name_resolution_2_0)
	{
	  auto nr_ctx
	    = Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

	  auto candidate = nr_ctx.values.to_rib (body_mappings.get_nodeid ());

	  rust_assert (candidate.has_value ());
	}
      else
	{
	  Resolver::Rib *rib = nullptr;
	  bool ok
	    = ctx->get_resolver ()->find_name_rib (body_mappings.get_nodeid (),
						   &rib);
	  rust_assert (ok);
	}
    }

  tree enclosing_scope = NULL_TREE;
  location_t start_location = function_body->get_locus ();
  location_t end_location = function_body->get_locus ();
  if (is_block_expr)
    {
      HIR::BlockExpr *body = static_cast<HIR::BlockExpr *> (function_body);
      start_location = body->get_locus ();
      end_location = body->get_end_locus ();
    }

  tree code_block = Backend::block (fndecl, enclosing_scope, {} /*locals*/,
				    start_location, end_location);
  ctx->push_block (code_block);

  TyTy::BaseType *tyret = &closure_tyty.get_result_type ();
  Bvariable *return_address = nullptr;

  tree return_type = TyTyResolveCompile::compile (ctx, tyret);
  bool address_is_taken = false;
  tree ret_var_stmt = NULL_TREE;

  return_address
    = Backend::temporary_variable (fndecl, code_block, return_type, NULL,
				   address_is_taken, expr.get_locus (),
				   &ret_var_stmt);

  ctx->add_statement (ret_var_stmt);

  ctx->push_fn (fndecl, return_address, tyret);

  if (is_block_expr)
    {
      HIR::BlockExpr *body = static_cast<HIR::BlockExpr *> (function_body);
      compile_function_body (fndecl, *body, tyret);
    }
  else
    {
      tree value = CompileExpr::Compile (function_body, ctx);
      tree return_expr
	= Backend::return_statement (fndecl, value,
				     function_body->get_locus ());
      ctx->add_statement (return_expr);
    }

  tree bind_tree = ctx->pop_block ();

  gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
  DECL_SAVED_TREE (fndecl) = bind_tree;

  ctx->pop_closure_context ();
  ctx->pop_fn ();
  ctx->push_function (fndecl);

  return fndecl;
}

tree
CompileExpr::generate_closure_fntype (HIR::ClosureExpr &expr,
				      const TyTy::ClosureType &closure_tyty,
				      tree compiled_closure_tyty,
				      TyTy::FnType **fn_tyty)
{
  // grab the specified_bound
  rust_assert (closure_tyty.num_specified_bounds () == 1);
  const TyTy::TypeBoundPredicate &predicate
    = *closure_tyty.get_specified_bounds ().begin ();

  // ensure the fn_once_output associated type is set
  closure_tyty.setup_fn_once_output ();

  // the function signature is based on the trait bound that the closure
  // implements which is determined at the type resolution time
  //
  // https://github.com/rust-lang/rust/blob/7807a694c2f079fd3f395821bcc357eee8650071/library/core/src/ops/function.rs#L54-L71

  TyTy::TypeBoundPredicateItem item = TyTy::TypeBoundPredicateItem::error ();
  if (predicate.get_name ().compare ("FnOnce") == 0)
    {
      item = predicate.lookup_associated_item ("call_once");
    }
  else if (predicate.get_name ().compare ("FnMut") == 0)
    {
      item = predicate.lookup_associated_item ("call_mut");
    }
  else if (predicate.get_name ().compare ("Fn") == 0)
    {
      item = predicate.lookup_associated_item ("call");
    }
  else
    {
      // FIXME error message?
      rust_unreachable ();
      return error_mark_node;
    }

  rust_assert (!item.is_error ());

  TyTy::BaseType *item_tyty = item.get_tyty_for_receiver (&closure_tyty);
  rust_assert (item_tyty->get_kind () == TyTy::TypeKind::FNDEF);
  *fn_tyty = static_cast<TyTy::FnType *> (item_tyty);
  return TyTyResolveCompile::compile (ctx, item_tyty);
}

bool
CompileExpr::generate_possible_fn_trait_call (HIR::CallExpr &expr,
					      tree receiver, tree *result)
{
  TyTy::FnType *fn_sig = nullptr;
  bool found_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fn_sig);
  if (!found_overload)
    return false;

  auto id = fn_sig->get_ty_ref ();
  auto dId = fn_sig->get_id ();

  tree function = error_mark_node;
  bool found_closure = ctx->lookup_function_decl (id, &function, dId, fn_sig);
  if (!found_closure)
    {
      // something went wrong we still return true as this was meant to be an fn
      // trait call
      *result = error_mark_node;
      return true;
    }

  // need to apply any autoderef's to the self argument
  HIR::Expr *fnexpr = expr.get_fnexpr ().get ();
  HirId autoderef_mappings_id = fnexpr->get_mappings ().get_hirid ();
  std::vector<Resolver::Adjustment> *adjustments = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_autoderef_mappings (autoderef_mappings_id,
							  &adjustments);
  rust_assert (ok);

  // apply adjustments for the fn call
  tree self = resolve_adjustements (*adjustments, receiver, expr.get_locus ());

  // resolve the arguments
  std::vector<tree> tuple_arg_vals;
  for (auto &argument : expr.get_arguments ())
    {
      auto rvalue = CompileExpr::Compile (argument.get (), ctx);
      tuple_arg_vals.push_back (rvalue);
    }

  // this is always the 2nd argument in the function signature
  tree fnty = TREE_TYPE (function);
  tree fn_arg_tys = TYPE_ARG_TYPES (fnty);
  tree tuple_args_tyty_chain = TREE_CHAIN (fn_arg_tys);
  tree tuple_args_tyty = TREE_VALUE (tuple_args_tyty_chain);

  tree tuple_args
    = Backend::constructor_expression (tuple_args_tyty, false, tuple_arg_vals,
				       -1, expr.get_locus ());

  // args are always self, and the tuple of the args we are passing where
  // self is the path of the call-expr in this case the fn_address
  std::vector<tree> args;
  args.push_back (self);
  args.push_back (tuple_args);

  tree call_address = address_expression (function, expr.get_locus ());
  *result
    = Backend::call_expression (call_address, args, nullptr /* static chain ?*/,
				expr.get_locus ());
  return true;
}

} // namespace Compile
} // namespace Rust
