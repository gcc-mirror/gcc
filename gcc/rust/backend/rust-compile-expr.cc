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

#include "rust-compile-expr.h"
#include "rust-compile-struct-field-expr.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir-path-probe.h"
#include "rust-hir-type-bounds.h"
#include "rust-compile-pattern.h"
#include "rust-compile-resolve-path.h"
#include "rust-compile-block.h"
#include "rust-compile-implitem.h"
#include "rust-constexpr.h"
#include "rust-unify.h"
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
    = ctx->get_backend ()->struct_field_expression (receiver_ref, index,
						    expr.get_locus ());
}

void
CompileExpr::visit (HIR::TupleExpr &expr)
{
  if (expr.is_unit ())
    {
      translated = ctx->get_backend ()->unit_expression ();
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

  translated
    = ctx->get_backend ()->constructor_expression (tuple_type, false, vals, -1,
						   expr.get_locus ());
}

void
CompileExpr::visit (HIR::ReturnExpr &expr)
{
  auto fncontext = ctx->peek_fn ();

  std::vector<tree> retstmts;
  if (expr.has_return_expr ())
    {
      tree compiled_expr = CompileExpr::Compile (expr.return_expr.get (), ctx);
      rust_assert (compiled_expr != nullptr);

      retstmts.push_back (compiled_expr);
    }

  auto s = ctx->get_backend ()->return_statement (fncontext.fndecl, retstmts,
						  expr.get_locus ());
  ctx->add_statement (s);
}

void
CompileExpr::visit (HIR::ArithmeticOrLogicalExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
  auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);

  // this might be an operator overload situation lets check
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type
	= Analysis::RustLangItem::OperatorToLangItem (expr.get_expr_type ());
      translated = resolve_operator_overload (lang_item_type, expr, lhs, rhs,
					      expr.get_lhs (), expr.get_rhs ());
      return;
    }

  if (ctx->in_fn () && !ctx->const_context_p ())
    {
      auto receiver_tmp = NULL_TREE;
      auto receiver
	= ctx->get_backend ()->temporary_variable (ctx->peek_fn ().fndecl,
						   NULL_TREE, TREE_TYPE (lhs),
						   lhs, true, expr.get_locus (),
						   &receiver_tmp);
      auto check
	= ctx->get_backend ()->arithmetic_or_logical_expression_checked (
	  op, lhs, rhs, expr.get_locus (), receiver);

      ctx->add_statement (check);
      translated = receiver->get_tree (expr.get_locus ());
    }
  else
    {
      translated = ctx->get_backend ()->arithmetic_or_logical_expression (
	op, lhs, rhs, expr.get_locus ());
    }
}

void
CompileExpr::visit (HIR::CompoundAssignmentExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto lhs = CompileExpr::Compile (expr.get_left_expr ().get (), ctx);
  auto rhs = CompileExpr::Compile (expr.get_right_expr ().get (), ctx);

  // this might be an operator overload situation lets check
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type
	= Analysis::RustLangItem::CompoundAssignmentOperatorToLangItem (
	  expr.get_expr_type ());
      auto compound_assignment
	= resolve_operator_overload (lang_item_type, expr, lhs, rhs,
				     expr.get_left_expr ().get (),
				     expr.get_right_expr ().get ());
      ctx->add_statement (compound_assignment);

      return;
    }

  if (ctx->in_fn () && !ctx->const_context_p ())
    {
      auto tmp = NULL_TREE;
      auto receiver
	= ctx->get_backend ()->temporary_variable (ctx->peek_fn ().fndecl,
						   NULL_TREE, TREE_TYPE (lhs),
						   lhs, true, expr.get_locus (),
						   &tmp);
      auto check
	= ctx->get_backend ()->arithmetic_or_logical_expression_checked (
	  op, lhs, rhs, expr.get_locus (), receiver);
      ctx->add_statement (check);

      translated = ctx->get_backend ()->assignment_statement (
	lhs, receiver->get_tree (expr.get_locus ()), expr.get_locus ());
    }
  else
    {
      translated = ctx->get_backend ()->arithmetic_or_logical_expression (
	op, lhs, rhs, expr.get_locus ());
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
      auto lang_item_type
	= Analysis::RustLangItem::NegationOperatorToLangItem (op);
      translated
	= resolve_operator_overload (lang_item_type, expr, negated_expr,
				     nullptr, expr.get_expr ().get (), nullptr);
      return;
    }

  translated
    = ctx->get_backend ()->negation_expression (op, negated_expr, location);
}

void
CompileExpr::visit (HIR::ComparisonExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
  auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);
  auto location = expr.get_locus ();

  translated
    = ctx->get_backend ()->comparison_expression (op, lhs, rhs, location);
}

void
CompileExpr::visit (HIR::LazyBooleanExpr &expr)
{
  auto op = expr.get_expr_type ();
  auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
  auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);
  auto location = expr.get_locus ();

  translated
    = ctx->get_backend ()->lazy_boolean_expression (op, lhs, rhs, location);
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
  bool needs_temp = !if_type->is_unit ();
  if (needs_temp)
    {
      fncontext fnctx = ctx->peek_fn ();
      tree enclosing_scope = ctx->peek_enclosing_scope ();
      tree block_type = TyTyResolveCompile::compile (ctx, if_type);

      bool is_address_taken = false;
      tree ret_var_stmt = nullptr;
      tmp = ctx->get_backend ()->temporary_variable (
	fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	expr.get_locus (), &ret_var_stmt);
      ctx->add_statement (ret_var_stmt);
    }

  auto stmt = CompileConditionalBlocks::compile (&expr, ctx, tmp);
  ctx->add_statement (stmt);

  if (tmp != NULL)
    {
      translated = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
    }
}

void
CompileExpr::visit (HIR::IfExprConseqIf &expr)
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
  bool needs_temp = !if_type->is_unit ();
  if (needs_temp)
    {
      fncontext fnctx = ctx->peek_fn ();
      tree enclosing_scope = ctx->peek_enclosing_scope ();
      tree block_type = TyTyResolveCompile::compile (ctx, if_type);

      bool is_address_taken = false;
      tree ret_var_stmt = nullptr;
      tmp = ctx->get_backend ()->temporary_variable (
	fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	expr.get_locus (), &ret_var_stmt);
      ctx->add_statement (ret_var_stmt);
    }

  auto stmt = CompileConditionalBlocks::compile (&expr, ctx, tmp);
  ctx->add_statement (stmt);

  if (tmp != NULL)
    {
      translated = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
    }
}

void
CompileExpr::visit (HIR::BlockExpr &expr)
{
  TyTy::BaseType *block_tyty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
				       &block_tyty))
    {
      rust_error_at (expr.get_locus (), "failed to lookup type of BlockExpr");
      return;
    }

  Bvariable *tmp = NULL;
  bool needs_temp = !block_tyty->is_unit ();
  if (needs_temp)
    {
      fncontext fnctx = ctx->peek_fn ();
      tree enclosing_scope = ctx->peek_enclosing_scope ();
      tree block_type = TyTyResolveCompile::compile (ctx, block_tyty);

      bool is_address_taken = false;
      tree ret_var_stmt = nullptr;
      tmp = ctx->get_backend ()->temporary_variable (
	fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	expr.get_locus (), &ret_var_stmt);
      ctx->add_statement (ret_var_stmt);
    }

  auto block_stmt = CompileBlock::compile (&expr, ctx, tmp);
  rust_assert (TREE_CODE (block_stmt) == BIND_EXPR);
  ctx->add_statement (block_stmt);

  if (tmp != NULL)
    {
      translated = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
    }
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
  translated = ctx->get_backend ()->unit_expression ();
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
      // base struct was specified those fields are filed via accesors
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

  translated = ctx->get_backend ()->constructor_expression (
    compiled_adt_type, adt->is_enum (), ctor_arguments, union_disriminator,
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
      bool ok
	= variant->lookup_field (expr.get_field_name (), nullptr, &field_index);
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
      bool ok
	= variant->lookup_field (expr.get_field_name (), nullptr, &field_index);
      rust_assert (ok);

      tree indirect = indirect_expression (receiver_ref, expr.get_locus ());
      receiver_ref = indirect;
    }

  translated
    = ctx->get_backend ()->struct_field_expression (receiver_ref, field_index,
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
  Bvariable *tmp = ctx->get_backend ()->temporary_variable (
    fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
    expr.get_locus (), &ret_var_stmt);
  ctx->add_statement (ret_var_stmt);
  ctx->push_loop_context (tmp);

  if (expr.has_loop_label ())
    {
      HIR::LoopLabel &loop_label = expr.get_loop_label ();
      tree label
	= ctx->get_backend ()->label (fnctx.fndecl,
				      loop_label.get_lifetime ().get_name (),
				      loop_label.get_locus ());
      tree label_decl = ctx->get_backend ()->label_definition_statement (label);
      ctx->add_statement (label_decl);
      ctx->insert_label_decl (
	loop_label.get_lifetime ().get_mappings ().get_hirid (), label);
    }

  tree loop_begin_label
    = ctx->get_backend ()->label (fnctx.fndecl, "", expr.get_locus ());
  tree loop_begin_label_decl
    = ctx->get_backend ()->label_definition_statement (loop_begin_label);
  ctx->add_statement (loop_begin_label_decl);
  ctx->push_loop_begin_label (loop_begin_label);

  tree code_block
    = CompileBlock::compile (expr.get_loop_block ().get (), ctx, nullptr);
  tree loop_expr
    = ctx->get_backend ()->loop_expression (code_block, expr.get_locus ());
  ctx->add_statement (loop_expr);

  ctx->pop_loop_context ();
  translated = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());

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
	= ctx->get_backend ()->label (fnctx.fndecl,
				      loop_label.get_lifetime ().get_name (),
				      loop_label.get_locus ());
      tree label_decl = ctx->get_backend ()->label_definition_statement (label);
      ctx->add_statement (label_decl);
      ctx->insert_label_decl (
	loop_label.get_lifetime ().get_mappings ().get_hirid (), label);
    }

  std::vector<Bvariable *> locals;
  Location start_location = expr.get_loop_block ()->get_locus ();
  Location end_location = expr.get_loop_block ()->get_locus (); // FIXME

  tree enclosing_scope = ctx->peek_enclosing_scope ();
  tree loop_block
    = ctx->get_backend ()->block (fnctx.fndecl, enclosing_scope, locals,
				  start_location, end_location);
  ctx->push_block (loop_block);

  tree loop_begin_label
    = ctx->get_backend ()->label (fnctx.fndecl, "", expr.get_locus ());
  tree loop_begin_label_decl
    = ctx->get_backend ()->label_definition_statement (loop_begin_label);
  ctx->add_statement (loop_begin_label_decl);
  ctx->push_loop_begin_label (loop_begin_label);

  tree condition
    = CompileExpr::Compile (expr.get_predicate_expr ().get (), ctx);
  tree exit_condition
    = fold_build1_loc (expr.get_locus ().gcc_location (), TRUTH_NOT_EXPR,
		       boolean_type_node, condition);
  tree exit_expr
    = ctx->get_backend ()->exit_expression (exit_condition, expr.get_locus ());
  ctx->add_statement (exit_expr);

  tree code_block_stmt
    = CompileBlock::compile (expr.get_loop_block ().get (), ctx, nullptr);
  rust_assert (TREE_CODE (code_block_stmt) == BIND_EXPR);
  ctx->add_statement (code_block_stmt);

  ctx->pop_loop_begin_label ();
  ctx->pop_block ();

  tree loop_expr
    = ctx->get_backend ()->loop_expression (loop_block, expr.get_locus ());
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
	= ctx->get_backend ()->var_expression (loop_result_holder,
					       expr.get_expr ()->get_locus ());

      tree assignment
	= ctx->get_backend ()->assignment_statement (result_reference,
						     compiled_expr,
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

      tree goto_label
	= ctx->get_backend ()->goto_statement (label, expr.get_locus ());
      ctx->add_statement (goto_label);
    }
  else
    {
      tree exit_expr = ctx->get_backend ()->exit_expression (
	ctx->get_backend ()->boolean_constant_expression (true),
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

  translated = ctx->get_backend ()->goto_statement (label, expr.get_locus ());
}

void
CompileExpr::visit (HIR::BorrowExpr &expr)
{
  tree main_expr = CompileExpr::Compile (expr.get_expr ().get (), ctx);
  if (SLICE_TYPE_P (TREE_TYPE (main_expr)))
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
      auto lang_item_type = Analysis::RustLangItem::ItemType::DEREF;
      tree operator_overload_call
	= resolve_operator_overload (lang_item_type, expr, main_expr, nullptr,
				     expr.get_expr ().get (), nullptr);

      // rust deref always returns a reference from this overload then we can
      // actually do the indirection
      main_expr = operator_overload_call;
    }

  tree expected_type = TyTyResolveCompile::compile (ctx, tyty);
  if (SLICE_TYPE_P (TREE_TYPE (main_expr)) && SLICE_TYPE_P (expected_type))
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
  auto lvalue = CompileExpr::Compile (expr.get_lhs (), ctx);
  auto rvalue = CompileExpr::Compile (expr.get_rhs (), ctx);

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

  tree assignment
    = ctx->get_backend ()->assignment_statement (lvalue, rvalue,
						 expr.get_locus ());

  ctx->add_statement (assignment);
}

// Helper for sort_tuple_patterns.
// Determine whether Patterns a and b are really the same pattern.
// FIXME: This is a nasty hack to avoid properly implementing a comparison
//        for Patterns, which we really probably do want at some point.
static bool
patterns_mergeable (HIR::Pattern *a, HIR::Pattern *b)
{
  if (!a || !b)
    return false;

  HIR::Pattern::PatternType pat_type = a->get_pattern_type ();
  if (b->get_pattern_type () != pat_type)
    return false;

  switch (pat_type)
    {
      case HIR::Pattern::PatternType::PATH: {
	// FIXME: this is far too naive
	HIR::PathPattern &aref = *static_cast<HIR::PathPattern *> (a);
	HIR::PathPattern &bref = *static_cast<HIR::PathPattern *> (b);
	if (aref.get_num_segments () != bref.get_num_segments ())
	  return false;

	const auto &asegs = aref.get_segments ();
	const auto &bsegs = bref.get_segments ();
	for (size_t i = 0; i < asegs.size (); i++)
	  {
	    if (asegs[i].as_string () != bsegs[i].as_string ())
	      return false;
	  }
	return true;
      }
      break;
      case HIR::Pattern::PatternType::LITERAL: {
	HIR::LiteralPattern &aref = *static_cast<HIR::LiteralPattern *> (a);
	HIR::LiteralPattern &bref = *static_cast<HIR::LiteralPattern *> (b);
	return aref.get_literal ().is_equal (bref.get_literal ());
      }
      break;
      case HIR::Pattern::PatternType::IDENTIFIER: {
	// TODO
      }
      break;
    case HIR::Pattern::PatternType::WILDCARD:
      return true;
      break;

      // TODO

    default:;
    }
  return false;
}

// A little container for rearranging the patterns and cases in a match
// expression while simplifying.
struct PatternMerge
{
  std::unique_ptr<HIR::MatchCase> wildcard;
  std::vector<std::unique_ptr<HIR::Pattern>> heads;
  std::vector<std::vector<HIR::MatchCase>> cases;
};

// Helper for simplify_tuple_match.
// For each tuple pattern in a given match, pull out the first elt of the
// tuple and construct a new MatchCase with the remaining tuple elts as the
// pattern. Return a mapping from each _unique_ first tuple element to a
// vec of cases for a new match.
//
// FIXME: This used to be a std::map<Pattern, Vec<MatchCase>>, but it doesn't
// actually work like we want - the Pattern includes an HIR ID, which is unique
// per Pattern object. This means we don't have a good means for comparing
// Patterns. It would probably be best to actually implement a means of
// properly comparing patterns, and then use an actual map.
//
static struct PatternMerge
sort_tuple_patterns (HIR::MatchExpr &expr)
{
  rust_assert (expr.get_scrutinee_expr ()->get_expression_type ()
	       == HIR::Expr::ExprType::Tuple);

  struct PatternMerge result;
  result.wildcard = nullptr;
  result.heads = std::vector<std::unique_ptr<HIR::Pattern>> ();
  result.cases = std::vector<std::vector<HIR::MatchCase>> ();

  for (auto &match_case : expr.get_match_cases ())
    {
      HIR::MatchArm &case_arm = match_case.get_arm ();

      // FIXME: Note we are only dealing with the first pattern in the arm.
      // The patterns vector in the arm might hold many patterns, which are the
      // patterns separated by the '|' token. Rustc abstracts these as "Or"
      // patterns, and part of its simplification process is to get rid of them.
      // We should get rid of the ORs too, maybe here or earlier than here?
      auto pat = case_arm.get_patterns ()[0]->clone_pattern ();

      // Record wildcards so we can add them in inner matches.
      if (pat->get_pattern_type () == HIR::Pattern::PatternType::WILDCARD)
	{
	  // The *whole* pattern is a wild card (_).
	  result.wildcard
	    = std::unique_ptr<HIR::MatchCase> (new HIR::MatchCase (match_case));
	  continue;
	}

      rust_assert (pat->get_pattern_type ()
		   == HIR::Pattern::PatternType::TUPLE);

      auto ref = *static_cast<HIR::TuplePattern *> (pat.get ());

      rust_assert (ref.has_tuple_pattern_items ());

      auto items
	= HIR::TuplePattern (ref).get_items ()->clone_tuple_pattern_items ();
      if (items->get_pattern_type ()
	  == HIR::TuplePatternItems::TuplePatternItemType::MULTIPLE)
	{
	  auto items_ref
	    = *static_cast<HIR::TuplePatternItemsMultiple *> (items.get ());

	  // Pop the first pattern out
	  auto patterns = std::vector<std::unique_ptr<HIR::Pattern>> ();
	  auto first = items_ref.get_patterns ()[0]->clone_pattern ();
	  for (auto p = items_ref.get_patterns ().begin () + 1;
	       p != items_ref.get_patterns ().end (); p++)
	    {
	      patterns.push_back ((*p)->clone_pattern ());
	    }

	  // if there is only one pattern left, don't make a tuple out of it
	  std::unique_ptr<HIR::Pattern> result_pattern;
	  if (patterns.size () == 1)
	    {
	      result_pattern = std::move (patterns[0]);
	    }
	  else
	    {
	      auto new_items = std::unique_ptr<HIR::TuplePatternItems> (
		new HIR::TuplePatternItemsMultiple (std::move (patterns)));

	      // Construct a TuplePattern from the rest of the patterns
	      result_pattern = std::unique_ptr<HIR::Pattern> (
		new HIR::TuplePattern (ref.get_pattern_mappings (),
				       std::move (new_items),
				       ref.get_locus ()));
	    }

	  // I don't know why we need to make foo separately here but
	  // using the { new_tuple } syntax in new_arm constructor does not
	  // compile.
	  auto foo = std::vector<std::unique_ptr<HIR::Pattern>> ();
	  foo.emplace_back (std::move (result_pattern));
	  HIR::MatchArm new_arm (std::move (foo), Location (), nullptr,
				 AST::AttrVec ());

	  HIR::MatchCase new_case (match_case.get_mappings (), new_arm,
				   match_case.get_expr ()->clone_expr ());

	  bool pushed = false;
	  for (size_t i = 0; i < result.heads.size (); i++)
	    {
	      if (patterns_mergeable (result.heads[i].get (), first.get ()))
		{
		  result.cases[i].push_back (new_case);
		  pushed = true;
		}
	    }

	  if (!pushed)
	    {
	      result.heads.push_back (std::move (first));
	      result.cases.push_back ({new_case});
	    }
	}
      else /* TuplePatternItemType::RANGED */
	{
	  // FIXME
	  gcc_unreachable ();
	}
    }

  return result;
}

// Helper for CompileExpr::visit (HIR::MatchExpr).
// Given a MatchExpr where the scrutinee is some kind of tuple, build an
// equivalent match where only one element of the tuple is examined at a time.
// This resulting match can then be lowered to a SWITCH_EXPR tree directly.
//
// The approach is as follows:
// 1. Split the scrutinee and each pattern into the first (head) and the
//    rest (tail).
// 2. Build a mapping of unique pattern heads to the cases (tail and expr)
//    that shared that pattern head in the original match.
//    (This is the job of sort_tuple_patterns ()).
// 3. For each unique pattern head, build a new MatchCase where the pattern
//    is the unique head, and the expression is a new match where:
//    - The scrutinee is the tail of the original scrutinee
//    - The cases are are those built by the mapping in step 2, i.e. the
//      tails of the patterns and the corresponing expressions from the
//      original match expression.
// 4. Do this recursively for each inner match, until there is nothing more
//    to simplify.
// 5. Build the resulting match which scrutinizes the head of the original
//    scrutinee, using the cases built in step 3.
static HIR::MatchExpr
simplify_tuple_match (HIR::MatchExpr &expr)
{
  if (expr.get_scrutinee_expr ()->get_expression_type ()
      != HIR::Expr::ExprType::Tuple)
    return expr;

  auto ref = *static_cast<HIR::TupleExpr *> (expr.get_scrutinee_expr ().get ());

  auto &tail = ref.get_tuple_elems ();
  rust_assert (tail.size () > 1);

  auto head = std::move (tail[0]);
  tail.erase (tail.begin (), tail.begin () + 1);

  // e.g.
  // match (tupA, tupB, tupC) {
  //   (a1, b1, c1) => { blk1 },
  //   (a2, b2, c2) => { blk2 },
  //   (a1, b3, c3) => { blk3 },
  // }
  // tail = (tupB, tupC)
  // head = tupA

  // Make sure the tail is only a tuple if it consists of at least 2 elements.
  std::unique_ptr<HIR::Expr> remaining;
  if (tail.size () == 1)
    remaining = std::move (tail[0]);
  else
    remaining = std::unique_ptr<HIR::Expr> (
      new HIR::TupleExpr (ref.get_mappings (), std::move (tail),
			  AST::AttrVec (), ref.get_outer_attrs (),
			  ref.get_locus ()));

  // e.g.
  // a1 -> [(b1, c1) => { blk1 },
  //        (b3, c3) => { blk3 }]
  // a2 -> [(b2, c2) => { blk2 }]
  struct PatternMerge map = sort_tuple_patterns (expr);

  std::vector<HIR::MatchCase> cases;
  // Construct the inner match for each unique first elt of the tuple
  // patterns
  for (size_t i = 0; i < map.heads.size (); i++)
    {
      auto inner_match_cases = map.cases[i];

      // If there is a wildcard at the outer match level, then need to
      // propegate the wildcard case into *every* inner match.
      // FIXME: It is probably not correct to add this unconditionally, what if
      // we have a pattern like (a, _, c)? Then there is already a wildcard in
      // the inner matches, and having two will cause two 'default:' blocks
      // which is an error.
      if (map.wildcard != nullptr)
	{
	  inner_match_cases.push_back (*(map.wildcard.get ()));
	}

      // match (tupB, tupC) {
      //   (b1, c1) => { blk1 },
      //   (b3, c3) => { blk3 }
      // }
      HIR::MatchExpr inner_match (expr.get_mappings (),
				  remaining->clone_expr (), inner_match_cases,
				  AST::AttrVec (), expr.get_outer_attrs (),
				  expr.get_locus ());

      inner_match = simplify_tuple_match (inner_match);

      auto outer_arm_pat = std::vector<std::unique_ptr<HIR::Pattern>> ();
      outer_arm_pat.emplace_back (map.heads[i]->clone_pattern ());

      HIR::MatchArm outer_arm (std::move (outer_arm_pat), expr.get_locus ());

      // Need to move the inner match to the heap and put it in a unique_ptr to
      // build the actual match case of the outer expression
      // auto inner_expr = std::unique_ptr<HIR::Expr> (new HIR::MatchExpr
      // (inner_match));
      auto inner_expr = inner_match.clone_expr ();

      // a1 => match (tupB, tupC) { ... }
      HIR::MatchCase outer_case (expr.get_mappings (), outer_arm,
				 std::move (inner_expr));

      cases.push_back (outer_case);
    }

  // If there was a wildcard, make sure to include it at the outer match level
  // too.
  if (map.wildcard != nullptr)
    {
      cases.push_back (*(map.wildcard.get ()));
    }

  // match tupA {
  //   a1 => match (tupB, tupC) {
  //     (b1, c1) => { blk1 },
  //     (b3, c3) => { blk3 }
  //   }
  //   a2 => match (tupB, tupC) {
  //     (b2, c2) => { blk2 }
  //   }
  // }
  HIR::MatchExpr outer_match (expr.get_mappings (), std::move (head), cases,
			      AST::AttrVec (), expr.get_outer_attrs (),
			      expr.get_locus ());

  return outer_match;
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
	       || scrutinee_kind == TyTy::TypeKind::TUPLE);

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
  bool needs_temp = !expr_tyty->is_unit ();
  if (needs_temp)
    {
      tree enclosing_scope = ctx->peek_enclosing_scope ();
      tree block_type = TyTyResolveCompile::compile (ctx, expr_tyty);

      bool is_address_taken = false;
      tree ret_var_stmt = nullptr;
      tmp = ctx->get_backend ()->temporary_variable (
	fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	expr.get_locus (), &ret_var_stmt);
      ctx->add_statement (ret_var_stmt);
    }

  // lets compile the scrutinee expression
  tree match_scrutinee_expr
    = CompileExpr::Compile (expr.get_scrutinee_expr ().get (), ctx);

  tree match_scrutinee_expr_qualifier_expr;
  if (TyTy::is_primitive_type_kind (scrutinee_kind))
    {
      match_scrutinee_expr_qualifier_expr = match_scrutinee_expr;
    }
  else if (scrutinee_kind == TyTy::TypeKind::ADT)
    {
      // need to access qualifier the field, if we use QUAL_UNION_TYPE this
      // would be DECL_QUALIFIER i think. For now this will just access the
      // first record field and its respective qualifier because it will always
      // be set because this is all a big special union
      tree scrutinee_first_record_expr
	= ctx->get_backend ()->struct_field_expression (
	  match_scrutinee_expr, 0, expr.get_scrutinee_expr ()->get_locus ());
      match_scrutinee_expr_qualifier_expr
	= ctx->get_backend ()->struct_field_expression (
	  scrutinee_first_record_expr, 0,
	  expr.get_scrutinee_expr ()->get_locus ());
    }
  else if (scrutinee_kind == TyTy::TypeKind::TUPLE)
    {
      // match on tuple becomes a series of nested switches, with one level
      // for each element of the tuple from left to right.
      auto exprtype = expr.get_scrutinee_expr ()->get_expression_type ();
      switch (exprtype)
	{
	  case HIR::Expr::ExprType::Tuple: {
	    // Build an equivalent expression which is nicer to lower.
	    HIR::MatchExpr outer_match = simplify_tuple_match (expr);

	    // We've rearranged the match into something that lowers better
	    // to GENERIC trees.
	    // For actually doing the lowering we need to compile the match
	    // we've just made. But we're half-way through compiling the
	    // original one.
	    // ...
	    // For now, let's just replace the original with the rearranged one
	    // we just made, and compile that instead. What could go wrong? :)
	    //
	    // FIXME: What about when we decide a temporary is needed above?
	    //        We might have already pushed a statement for it that
	    //        we no longer need. Probably need to rearrange the order
	    //        of these steps.
	    expr = outer_match;

	    scrutinee_kind = check_match_scrutinee (expr, ctx);
	    if (scrutinee_kind == TyTy::TypeKind::ERROR)
	      {
		translated = error_mark_node;
		return;
	      }

	    // Now compile the scrutinee of the simplified match.
	    // FIXME: this part is duplicated from above.
	    match_scrutinee_expr
	      = CompileExpr::Compile (expr.get_scrutinee_expr ().get (), ctx);

	    if (TyTy::is_primitive_type_kind (scrutinee_kind))
	      {
		match_scrutinee_expr_qualifier_expr = match_scrutinee_expr;
	      }
	    else if (scrutinee_kind == TyTy::TypeKind::ADT)
	      {
		// need to access qualifier the field, if we use QUAL_UNION_TYPE
		// this would be DECL_QUALIFIER i think. For now this will just
		// access the first record field and its respective qualifier
		// because it will always be set because this is all a big
		// special union
		tree scrutinee_first_record_expr
		  = ctx->get_backend ()->struct_field_expression (
		    match_scrutinee_expr, 0,
		    expr.get_scrutinee_expr ()->get_locus ());
		match_scrutinee_expr_qualifier_expr
		  = ctx->get_backend ()->struct_field_expression (
		    scrutinee_first_record_expr, 0,
		    expr.get_scrutinee_expr ()->get_locus ());
	      }
	    else
	      {
		// FIXME: There are other cases, but it better not be a Tuple
		gcc_unreachable ();
	      }
	  }
	  break;

	  case HIR::Expr::ExprType::Path: {
	    // FIXME
	    gcc_unreachable ();
	  }
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      // FIXME: match on other types of expressions not yet implemented.
      gcc_unreachable ();
    }

  // setup the end label so the cases can exit properly
  tree fndecl = fnctx.fndecl;
  Location end_label_locus = expr.get_locus (); // FIXME
  tree end_label
    = ctx->get_backend ()->label (fndecl,
				  "" /* empty creates an artificial label */,
				  end_label_locus);
  tree end_label_decl_statement
    = ctx->get_backend ()->label_definition_statement (end_label);

  // setup the switch-body-block
  Location start_location; // FIXME
  Location end_location;   // FIXME
  tree enclosing_scope = ctx->peek_enclosing_scope ();
  tree switch_body_block
    = ctx->get_backend ()->block (fndecl, enclosing_scope, {}, start_location,
				  end_location);
  ctx->push_block (switch_body_block);

  for (auto &kase : expr.get_match_cases ())
    {
      // for now lets just get single pattern's working
      HIR::MatchArm &kase_arm = kase.get_arm ();
      rust_assert (kase_arm.get_patterns ().size () > 0);

      // generate implicit label
      Location arm_locus = kase_arm.get_locus ();
      tree case_label = ctx->get_backend ()->label (
	fndecl, "" /* empty creates an artificial label */, arm_locus);

      // setup the bindings for the block
      for (auto &kase_pattern : kase_arm.get_patterns ())
	{
	  tree switch_kase_expr
	    = CompilePatternCaseLabelExpr::Compile (kase_pattern.get (),
						    case_label, ctx);
	  ctx->add_statement (switch_kase_expr);

	  CompilePatternBindings::Compile (kase_pattern.get (),
					   match_scrutinee_expr, ctx);
	}

      // compile the expr and setup the assignment if required when tmp != NULL
      tree kase_expr_tree = CompileExpr::Compile (kase.get_expr ().get (), ctx);
      if (tmp != NULL)
	{
	  tree result_reference
	    = ctx->get_backend ()->var_expression (tmp, arm_locus);
	  tree assignment
	    = ctx->get_backend ()->assignment_statement (result_reference,
							 kase_expr_tree,
							 arm_locus);
	  ctx->add_statement (assignment);
	}

      // go to end label
      tree goto_end_label = build1_loc (arm_locus.gcc_location (), GOTO_EXPR,
					void_type_node, end_label);
      ctx->add_statement (goto_end_label);
    }

  // setup the switch expression
  tree match_body = ctx->pop_block ();
  tree match_expr_stmt
    = build2_loc (expr.get_locus ().gcc_location (), SWITCH_EXPR,
		  TREE_TYPE (match_scrutinee_expr_qualifier_expr),
		  match_scrutinee_expr_qualifier_expr, match_body);
  ctx->add_statement (match_expr_stmt);
  ctx->add_statement (end_label_decl_statement);

  if (tmp != NULL)
    {
      translated = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
    }
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
      // base struct was specified those fields are filed via accesors
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
	  Location lvalue_locus
	    = ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
	  Location rvalue_locus = argument->get_locus ();
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

      translated = ctx->get_backend ()->constructor_expression (
	compiled_adt_type, adt->is_enum (), ctor_arguments, union_disriminator,
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
	*result = fn->param_at (index);

	return true;
      }

    const TyTy::FnType *fn = static_cast<const TyTy::FnType *> (base);
    auto param = fn->param_at (index);
    *result = param.second;

    return true;
  };

  auto fn_address = CompileExpr::Compile (expr.get_fnexpr (), ctx);

  // is this a closure call?
  bool possible_trait_call
    = generate_possible_fn_trait_call (expr, fn_address, &translated);
  if (possible_trait_call)
    return;

  bool is_varadic = false;
  if (tyty->get_kind () == TyTy::TypeKind::FNDEF)
    {
      const TyTy::FnType *fn = static_cast<const TyTy::FnType *> (tyty);
      is_varadic = fn->is_varadic ();
    }

  size_t required_num_args = expr.get_arguments ().size ();
  if (tyty->get_kind () == TyTy::TypeKind::FNDEF)
    {
      const TyTy::FnType *fn = static_cast<const TyTy::FnType *> (tyty);
      required_num_args = fn->num_params ();
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

      if (is_varadic && i >= required_num_args)
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
      Location lvalue_locus
	= ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
      Location rvalue_locus = argument->get_locus ();
      rvalue = coercion_site (argument->get_mappings ().get_hirid (), rvalue,
			      actual, expected, lvalue_locus, rvalue_locus);

      // add it to the list
      args.push_back (rvalue);
    }

  // must be a regular call to a function
  translated = ctx->get_backend ()->call_expression (fn_address, args, nullptr,
						     expr.get_locus ());
}

void
CompileExpr::visit (HIR::MethodCallExpr &expr)
{
  // method receiver
  tree self = CompileExpr::Compile (expr.get_receiver ().get (), ctx);

  // lookup the resolved name
  NodeId resolved_node_id = UNKNOWN_NODEID;
  if (!ctx->get_resolver ()->lookup_resolved_name (
	expr.get_mappings ().get_nodeid (), &resolved_node_id))
    {
      rust_error_at (expr.get_locus (), "failed to lookup resolved MethodCall");
      return;
    }

  // reverse lookup
  HirId ref;
  if (!ctx->get_mappings ()->lookup_node_to_hir (resolved_node_id, &ref))
    {
      rust_fatal_error (expr.get_locus (), "reverse lookup failure");
      return;
    }

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
      fn_expr
	= resolve_method_address (fntype, ref, receiver, segment_name,
				  expr.get_mappings (), expr.get_locus ());
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
      Location lvalue_locus
	= ctx->get_mappings ()->lookup_location (expected->get_ty_ref ());
      Location rvalue_locus = argument->get_locus ();
      rvalue = coercion_site (argument->get_mappings ().get_hirid (), rvalue,
			      actual, expected, lvalue_locus, rvalue_locus);

      // add it to the list
      args.push_back (rvalue);
    }

  translated = ctx->get_backend ()->call_expression (fn_expr, args, nullptr,
						     expr.get_locus ());
}

tree
CompileExpr::get_fn_addr_from_dyn (const TyTy::DynamicObjectType *dyn,
				   TyTy::BaseType *receiver,
				   TyTy::FnType *fntype, tree receiver_ref,
				   Location expr_locus)
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

  // get any indirection sorted out
  if (receiver->get_kind () == TyTy::TypeKind::REF)
    {
      tree indirect = indirect_expression (receiver_ref, expr_locus);
      receiver_ref = indirect;
    }

  // cast it to the correct fntype
  tree expected_fntype = TyTyResolveCompile::compile (ctx, fntype, true);
  tree idx = build_int_cst (size_type_node, offs);

  tree vtable_ptr
    = ctx->get_backend ()->struct_field_expression (receiver_ref, 1,
						    expr_locus);
  tree vtable_array_access = build4_loc (expr_locus.gcc_location (), ARRAY_REF,
					 TREE_TYPE (TREE_TYPE (vtable_ptr)),
					 vtable_ptr, idx, NULL_TREE, NULL_TREE);

  tree vcall
    = build3_loc (expr_locus.gcc_location (), OBJ_TYPE_REF, expected_fntype,
		  vtable_array_access, receiver_ref, idx);

  return vcall;
}

tree
CompileExpr::get_receiver_from_dyn (const TyTy::DynamicObjectType *dyn,
				    TyTy::BaseType *receiver,
				    TyTy::FnType *fntype, tree receiver_ref,
				    Location expr_locus)
{
  // get any indirection sorted out
  if (receiver->get_kind () == TyTy::TypeKind::REF)
    {
      tree indirect = indirect_expression (receiver_ref, expr_locus);
      receiver_ref = indirect;
    }

  // access the offs + 1 for the fnptr and offs=0 for the reciever obj
  return ctx->get_backend ()->struct_field_expression (receiver_ref, 0,
						       expr_locus);
}

tree
CompileExpr::resolve_method_address (TyTy::FnType *fntype, HirId ref,
				     TyTy::BaseType *receiver,
				     HIR::PathIdentSegment &segment,
				     Analysis::NodeMapping expr_mappings,
				     Location expr_locus)
{
  // lookup compiled functions since it may have already been compiled
  tree fn = NULL_TREE;
  if (ctx->lookup_function_decl (fntype->get_ty_ref (), &fn))
    {
      return address_expression (fn, expr_locus);
    }

  // Now we can try and resolve the address since this might be a forward
  // declared function, generic function which has not be compiled yet or
  // its an not yet trait bound function
  HIR::ImplItem *resolved_item
    = ctx->get_mappings ()->lookup_hir_implitem (ref, nullptr);
  if (resolved_item != nullptr)
    {
      if (!fntype->has_subsititions_defined ())
	return CompileInherentImplItem::Compile (resolved_item, ctx);

      return CompileInherentImplItem::Compile (resolved_item, ctx, fntype);
    }

  // it might be resolved to a trait item
  HIR::TraitItem *trait_item
    = ctx->get_mappings ()->lookup_hir_trait_item (ref);
  HIR::Trait *trait = ctx->get_mappings ()->lookup_trait_item_mapping (
    trait_item->get_mappings ().get_hirid ());

  Resolver::TraitReference *trait_ref
    = &Resolver::TraitReference::error_node ();
  bool ok = ctx->get_tyctx ()->lookup_trait_reference (
    trait->get_mappings ().get_defid (), &trait_ref);
  rust_assert (ok);

  // the type resolver can only resolve type bounds to their trait
  // item so its up to us to figure out if this path should resolve
  // to an trait-impl-block-item or if it can be defaulted to the
  // trait-impl-item's definition

  auto root = receiver->get_root ();
  auto candidates
    = Resolver::PathProbeType::Probe (root, segment, true /* probe_impls */,
				      false /* probe_bounds */,
				      true /* ignore_mandatory_trait_items */);
  if (candidates.size () == 0)
    {
      // this means we are defaulting back to the trait_item if
      // possible
      Resolver::TraitItemReference *trait_item_ref = nullptr;
      bool ok = trait_ref->lookup_hir_trait_item (*trait_item, &trait_item_ref);
      rust_assert (ok);				    // found
      rust_assert (trait_item_ref->is_optional ()); // has definition

      // FIXME Optional means it has a definition and an associated
      // block which can be a default implementation, if it does not
      // contain an implementation we should actually return
      // error_mark_node

      return CompileTraitItem::Compile (trait_item_ref->get_hir_trait_item (),
					ctx, fntype, true, expr_locus);
    }
  else
    {
      // FIXME this will be a case to return error_mark_node, there is
      // an error scenario where a Trait Foo has a method Bar, but this
      // receiver does not implement this trait or has an incompatible
      // implementation and we should just return error_mark_node

      rust_assert (candidates.size () == 1);
      auto &candidate = *candidates.begin ();
      rust_assert (candidate.is_impl_candidate ());
      rust_assert (candidate.ty->get_kind () == TyTy::TypeKind::FNDEF);
      TyTy::FnType *candidate_call = static_cast<TyTy::FnType *> (candidate.ty);

      HIR::ImplItem *impl_item = candidate.item.impl.impl_item;
      if (!candidate_call->has_subsititions_defined ())
	return CompileInherentImplItem::Compile (impl_item, ctx);

      TyTy::BaseType *monomorphized = candidate_call;
      if (candidate_call->needs_generic_substitutions ())
	{
	  TyTy::BaseType *infer_impl_call
	    = candidate_call->infer_substitions (expr_locus);
	  monomorphized = Resolver::UnifyRules::Resolve (
	    TyTy::TyWithLocation (infer_impl_call),
	    TyTy::TyWithLocation (fntype), expr_locus, true /* commit */,
	    true /* emit_errors */);
	}

      return CompileInherentImplItem::Compile (impl_item, ctx, monomorphized);
    }
}

tree
CompileExpr::resolve_operator_overload (
  Analysis::RustLangItem::ItemType lang_item_type, HIR::OperatorExprMeta expr,
  tree lhs, tree rhs, HIR::Expr *lhs_expr, HIR::Expr *rhs_expr)
{
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  rust_assert (is_op_overload);

  // lookup the resolved name
  NodeId resolved_node_id = UNKNOWN_NODEID;
  bool ok = ctx->get_resolver ()->lookup_resolved_name (
    expr.get_mappings ().get_nodeid (), &resolved_node_id);
  rust_assert (ok);

  // reverse lookup
  HirId ref;
  ok = ctx->get_mappings ()->lookup_node_to_hir (resolved_node_id, &ref);
  rust_assert (ok);

  TyTy::BaseType *receiver = nullptr;
  ok = ctx->get_tyctx ()->lookup_receiver (expr.get_mappings ().get_hirid (),
					   &receiver);
  rust_assert (ok);

  bool is_generic_receiver = receiver->get_kind () == TyTy::TypeKind::PARAM;
  if (is_generic_receiver)
    {
      TyTy::ParamType *p = static_cast<TyTy::ParamType *> (receiver);
      receiver = p->resolve ();
    }

  // lookup compiled functions since it may have already been compiled
  HIR::PathIdentSegment segment_name (
    Analysis::RustLangItem::ToString (lang_item_type));
  tree fn_expr
    = resolve_method_address (fntype, ref, receiver, segment_name,
			      expr.get_mappings (), expr.get_locus ());

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

  return ctx->get_backend ()->call_expression (fn_expr, args, nullptr,
					       expr.get_locus ());
}

tree
CompileExpr::compile_bool_literal (const HIR::LiteralExpr &expr,
				   const TyTy::BaseType *tyty)
{
  rust_assert (expr.get_lit_type () == HIR::Literal::BOOL);

  const auto literal_value = expr.get_literal ();
  bool bval = literal_value.as_string ().compare ("true") == 0;
  return ctx->get_backend ()->boolean_constant_expression (bval);
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
		     "integer overflows the respective type %<%s%>",
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
		     "decimal overflows the respective type %<%s%>",
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
  return ctx->get_backend ()->wchar_constant_expression (c);
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

  auto base = ctx->get_backend ()->string_constant_expression (
    literal_value.as_string ());
  tree data = address_expression (base, expr.get_locus ());

  TyTy::BaseType *usize = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_builtin ("usize", &usize);
  rust_assert (ok);
  tree type = TyTyResolveCompile::compile (ctx, usize);

  tree size = build_int_cstu (type, literal_value.as_string ().size ());

  return ctx->get_backend ()->constructor_expression (fat_pointer, false,
						      {data, size}, -1,
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
      tree bb = ctx->get_backend ()->char_constant_expression (b);
      vals.push_back (bb);
      indexes.push_back (i);
    }

  tree array_type = TyTyResolveCompile::compile (ctx, array_tyty);
  tree constructed
    = ctx->get_backend ()->array_constructor_expression (array_type, indexes,
							 vals,
							 expr.get_locus ());

  return address_expression (constructed, expr.get_locus ());
}

tree
CompileExpr::type_cast_expression (tree type_to_cast_to, tree expr_tree,
				   Location location)
{
  if (type_to_cast_to == error_mark_node || expr_tree == error_mark_node
      || TREE_TYPE (expr_tree) == error_mark_node)
    return error_mark_node;

  if (ctx->get_backend ()->type_size (type_to_cast_to) == 0
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
      return fold_build1_loc (location.gcc_location (), VIEW_CONVERT_EXPR,
			      type_to_cast_to, expr_tree);
    }
  else if (TREE_CODE (type_to_cast_to) == POINTER_TYPE
	   && SLICE_TYPE_P (TREE_TYPE (expr_tree)))
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
      return ctx->get_backend ()->struct_field_expression (expr_tree, 0,
							   location);
    }

  return fold_convert_loc (location.gcc_location (), type_to_cast_to,
			   expr_tree);
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
CompileExpr::array_value_expr (Location expr_locus,
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

  return ctx->get_backend ()->array_constructor_expression (array_type, indexes,
							    constructor,
							    expr_locus);
}

tree
CompileExpr::array_copied_expr (Location expr_locus,
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
  tree capacity_expr = CompileExpr::Compile (elems.get_num_copies_expr (), ctx);
  ctx->pop_const_context ();

  if (!TREE_CONSTANT (capacity_expr))
    {
      rust_error_at (expr_locus, "non const num copies %qT", array_type);
      return error_mark_node;
    }

  // get the compiled value
  tree translated_expr = CompileExpr::Compile (elems.get_elem_to_copy (), ctx);

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

      return ctx->get_backend ()->array_constructor_expression (array_type,
								indexes,
								constructor,
								expr_locus);
    }

  else
    {
      // Create a new block scope in which to initialize the array
      tree fndecl = NULL_TREE;
      if (ctx->in_fn ())
	fndecl = ctx->peek_fn ().fndecl;

      std::vector<Bvariable *> locals;
      tree enclosing_scope = ctx->peek_enclosing_scope ();
      tree init_block
	= ctx->get_backend ()->block (fndecl, enclosing_scope, locals,
				      expr_locus, expr_locus);
      ctx->push_block (init_block);

      tree tmp;
      tree stmts
	= ctx->get_backend ()->array_initializer (fndecl, init_block,
						  array_type, capacity_expr,
						  translated_expr, &tmp,
						  expr_locus);
      ctx->add_statement (stmts);

      tree block = ctx->pop_block ();

      // The result is a compound expression which creates a temporary array,
      // initializes all the elements in a loop, and then yeilds the array.
      return ctx->get_backend ()->compound_expression (block, tmp, expr_locus);
    }
}

tree
HIRCompileBase::resolve_adjustements (
  std::vector<Resolver::Adjustment> &adjustments, tree expression,
  Location locus)
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
	    if (!SLICE_TYPE_P (TREE_TYPE (e)))
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
					  tree expression, Location locus)
{
  rust_assert (adjustment.is_deref_adjustment ()
	       || adjustment.is_deref_mut_adjustment ());
  rust_assert (adjustment.has_operator_overload ());

  TyTy::FnType *lookup = adjustment.get_deref_operator_fn ();
  HIR::ImplItem *resolved_item = adjustment.get_deref_hir_item ();

  tree fn_address = error_mark_node;
  if (!lookup->has_subsititions_defined ())
    fn_address = CompileInherentImplItem::Compile (resolved_item, ctx, nullptr,
						   true, locus);
  else
    fn_address = CompileInherentImplItem::Compile (resolved_item, ctx, lookup,
						   true, locus);

  // does it need a reference to call
  tree adjusted_argument = expression;
  bool needs_borrow = adjustment.get_deref_adjustment_type ()
		      != Resolver::Adjustment::AdjustmentType::ERROR;
  if (needs_borrow)
    {
      adjusted_argument = address_expression (expression, locus);
    }

  // make the call
  return ctx->get_backend ()->call_expression (fn_address, {adjusted_argument},
					       nullptr, locus);
}

tree
HIRCompileBase::resolve_indirection_adjustment (
  Resolver::Adjustment &adjustment, tree expression, Location locus)
{
  return indirect_expression (expression, locus);
}

tree
HIRCompileBase::resolve_unsized_adjustment (Resolver::Adjustment &adjustment,
					    tree expression, Location locus)
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
  Resolver::Adjustment &adjustment, tree expression, Location locus)
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

  return ctx->get_backend ()->constructor_expression (fat_pointer, false,
						      {data, size}, -1, locus);
}

tree
HIRCompileBase::resolve_unsized_dyn_adjustment (
  Resolver::Adjustment &adjustment, tree expression, Location locus)
{
  tree rvalue = expression;
  Location rvalue_locus = locus;

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
  translated
    = ctx->get_backend ()->constructor_expression (adt, false, {from, to}, -1,
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
  translated
    = ctx->get_backend ()->constructor_expression (adt, false, {from}, -1,
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
    = ctx->get_backend ()->constructor_expression (adt, false, {to}, -1,
						   expr.get_locus ());
}

void
CompileExpr::visit (HIR::RangeFullExpr &expr)
{
  TyTy::BaseType *tyty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (), &tyty);
  rust_assert (ok);

  tree adt = TyTyResolveCompile::compile (ctx, tyty);
  translated = ctx->get_backend ()->constructor_expression (adt, false, {}, -1,
							    expr.get_locus ());
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
  translated
    = ctx->get_backend ()->constructor_expression (adt, false, {from, to}, -1,
						   expr.get_locus ());
}

void
CompileExpr::visit (HIR::ArrayIndexExpr &expr)
{
  tree array_reference = CompileExpr::Compile (expr.get_array_expr (), ctx);
  tree index = CompileExpr::Compile (expr.get_index_expr (), ctx);

  // this might be an core::ops::index lang item situation
  TyTy::FnType *fntype;
  bool is_op_overload = ctx->get_tyctx ()->lookup_operator_overload (
    expr.get_mappings ().get_hirid (), &fntype);
  if (is_op_overload)
    {
      auto lang_item_type = Analysis::RustLangItem::ItemType::INDEX;
      tree operator_overload_call
	= resolve_operator_overload (lang_item_type, expr, array_reference,
				     index, expr.get_array_expr (),
				     expr.get_index_expr ());

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

  translated
    = ctx->get_backend ()->array_index_expression (array_reference, index,
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

  translated
    = ctx->get_backend ()->constructor_expression (compiled_closure_tyty, false,
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
  Resolver::CanonicalPath path = parent_canonical_path.append (
    Resolver::CanonicalPath::new_seg (UNKNOWN_NODEID, "{{closure}}"));

  std::string ir_symbol_name = path.get ();
  std::string asm_name = ctx->mangle_item (&closure_tyty, path);

  unsigned int flags = 0;
  tree fndecl
    = ctx->get_backend ()->function (compiled_fn_type, ir_symbol_name, asm_name,
				     flags, expr.get_locus ());

  // insert into the context
  ctx->insert_function_decl (fn_tyty, fndecl);
  ctx->insert_closure_decl (&closure_tyty, fndecl);

  // setup the parameters
  std::vector<Bvariable *> param_vars;

  // closure self
  Bvariable *self_param
    = ctx->get_backend ()->parameter_variable (fndecl, "$closure",
					       compiled_closure_tyty,
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
      tree binding = ctx->get_backend ()->struct_field_expression (
	self_param->get_tree (expr.get_locus ()), idx, expr.get_locus ());
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
    = ctx->get_backend ()->parameter_variable (fndecl, "args", args_type,
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
      tree compiled_param_var = ctx->get_backend ()->struct_field_expression (
	args_param_expr, i, closure_param.get_locus ());

      const HIR::Pattern &param_pattern = *closure_param.get_pattern ();
      ctx->insert_pattern_binding (
	param_pattern.get_pattern_mappings ().get_hirid (), compiled_param_var);
      i++;
    }

  if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
    {
      ctx->pop_closure_context ();
      return error_mark_node;
    }

  // lookup locals
  HIR::Expr *function_body = expr.get_expr ().get ();
  auto body_mappings = function_body->get_mappings ();
  Resolver::Rib *rib = nullptr;
  bool ok
    = ctx->get_resolver ()->find_name_rib (body_mappings.get_nodeid (), &rib);
  rust_assert (ok);

  std::vector<Bvariable *> locals
    = compile_locals_for_block (ctx, *rib, fndecl);

  tree enclosing_scope = NULL_TREE;
  Location start_location = function_body->get_locus ();
  Location end_location = function_body->get_locus ();
  bool is_block_expr
    = function_body->get_expression_type () == HIR::Expr::ExprType::Block;
  if (is_block_expr)
    {
      HIR::BlockExpr *body = static_cast<HIR::BlockExpr *> (function_body);
      start_location = body->get_locus ();
      end_location = body->get_end_locus ();
    }

  tree code_block = ctx->get_backend ()->block (fndecl, enclosing_scope, locals,
						start_location, end_location);
  ctx->push_block (code_block);

  TyTy::BaseType *tyret = &closure_tyty.get_result_type ();
  bool function_has_return = !closure_tyty.get_result_type ().is_unit ();
  Bvariable *return_address = nullptr;
  if (function_has_return)
    {
      tree return_type = TyTyResolveCompile::compile (ctx, tyret);

      bool address_is_taken = false;
      tree ret_var_stmt = NULL_TREE;

      return_address = ctx->get_backend ()->temporary_variable (
	fndecl, code_block, return_type, NULL, address_is_taken,
	expr.get_locus (), &ret_var_stmt);

      ctx->add_statement (ret_var_stmt);
    }

  ctx->push_fn (fndecl, return_address);

  if (is_block_expr)
    {
      HIR::BlockExpr *body = static_cast<HIR::BlockExpr *> (function_body);
      compile_function_body (ctx, fndecl, *body, true);
    }
  else
    {
      tree value = CompileExpr::Compile (function_body, ctx);
      tree return_expr
	= ctx->get_backend ()->return_statement (fndecl, {value},
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
      gcc_unreachable ();
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
  HirId autoderef_mappings_id = expr.get_mappings ().get_hirid ();
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
    = ctx->get_backend ()->constructor_expression (tuple_args_tyty, false,
						   tuple_arg_vals, -1,
						   expr.get_locus ());

  // args are always self, and the tuple of the args we are passing where
  // self is the path of the call-expr in this case the fn_address
  std::vector<tree> args;
  args.push_back (self);
  args.push_back (tuple_args);

  tree call_address = address_expression (function, expr.get_locus ());
  *result = ctx->get_backend ()->call_expression (call_address, args,
						  nullptr /* static chain ?*/,
						  expr.get_locus ());
  return true;
}

} // namespace Compile
} // namespace Rust
