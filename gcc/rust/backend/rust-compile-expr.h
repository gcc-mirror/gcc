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

#ifndef RUST_COMPILE_EXPR
#define RUST_COMPILE_EXPR

#include "rust-compile-base.h"
#include "rust-compile-resolve-path.h"
#include "rust-compile-block.h"
#include "rust-compile-struct-field-expr.h"
#include "rust-constexpr.h"

namespace Rust {
namespace Compile {

class CompileExpr : public HIRCompileBase, public HIR::HIRExpressionVisitor
{
public:
  static tree Compile (HIR::Expr *expr, Context *ctx)
  {
    CompileExpr compiler (ctx);
    expr->accept_vis (compiler);
    return compiler.translated;
  }

  void visit (HIR::TupleIndexExpr &expr) override
  {
    HIR::Expr *tuple_expr = expr.get_tuple_expr ().get ();
    TupleIndex index = expr.get_tuple_index ();

    tree receiver_ref = CompileExpr::Compile (tuple_expr, ctx);

    TyTy::BaseType *tuple_expr_ty = nullptr;
    bool ok = ctx->get_tyctx ()->lookup_type (
      tuple_expr->get_mappings ().get_hirid (), &tuple_expr_ty);
    rust_assert (ok);

    // do we need to add an indirect reference
    if (tuple_expr_ty->get_kind () == TyTy::TypeKind::REF)
      {
	TyTy::ReferenceType *r
	  = static_cast<TyTy::ReferenceType *> (tuple_expr_ty);
	TyTy::BaseType *tuple_type = r->get_base ();
	tree tuple_tyty = TyTyResolveCompile::compile (ctx, tuple_type);

	tree indirect
	  = ctx->get_backend ()->indirect_expression (tuple_tyty, receiver_ref,
						      true, expr.get_locus ());
	receiver_ref = indirect;
      }

    translated
      = ctx->get_backend ()->struct_field_expression (receiver_ref, index,
						      expr.get_locus ());
  }

  void visit (HIR::TupleExpr &expr) override
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
      = ctx->get_backend ()->constructor_expression (tuple_type, false, vals,
						     -1, expr.get_locus ());
  }

  void visit (HIR::ReturnExpr &expr) override
  {
    auto fncontext = ctx->peek_fn ();

    std::vector<tree> retstmts;
    if (expr.has_return_expr ())
      {
	tree compiled_expr
	  = CompileExpr::Compile (expr.return_expr.get (), ctx);
	rust_assert (compiled_expr != nullptr);

	retstmts.push_back (compiled_expr);
      }

    auto s = ctx->get_backend ()->return_statement (fncontext.fndecl, retstmts,
						    expr.get_locus ());
    ctx->add_statement (s);
  }

  void visit (HIR::CallExpr &expr) override;

  void visit (HIR::MethodCallExpr &expr) override;

  void visit (HIR::IdentifierExpr &expr) override;

  void visit (HIR::LiteralExpr &expr) override
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

  void visit (HIR::AssignmentExpr &expr) override
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

    rvalue
      = coercion_site (rvalue, actual, expected, expr.get_lhs ()->get_locus (),
		       expr.get_rhs ()->get_locus ());

    tree assignment
      = ctx->get_backend ()->assignment_statement (lvalue, rvalue,
						   expr.get_locus ());

    ctx->add_statement (assignment);
  }

  void visit (HIR::CompoundAssignmentExpr &expr) override;

  void visit (HIR::ArrayIndexExpr &expr) override;

  void visit (HIR::ArrayExpr &expr) override;

  void visit (HIR::ArithmeticOrLogicalExpr &expr) override;

  void visit (HIR::ComparisonExpr &expr) override
  {
    auto op = expr.get_expr_type ();
    auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);
    auto location = expr.get_locus ();

    translated
      = ctx->get_backend ()->comparison_expression (op, lhs, rhs, location);
  }

  void visit (HIR::LazyBooleanExpr &expr) override
  {
    auto op = expr.get_expr_type ();
    auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);
    auto location = expr.get_locus ();

    translated
      = ctx->get_backend ()->lazy_boolean_expression (op, lhs, rhs, location);
  }

  void visit (HIR::NegationExpr &expr) override;

  void visit (HIR::TypeCastExpr &expr) override
  {
    TyTy::BaseType *tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &tyty))
      {
	translated = error_mark_node;
	return;
      }

    auto type_to_cast_to = TyTyResolveCompile::compile (ctx, tyty);
    auto casted_expr
      = CompileExpr::Compile (expr.get_casted_expr ().get (), ctx);
    translated
      = type_cast_expression (type_to_cast_to, casted_expr, expr.get_locus ());
  }

  void visit (HIR::IfExpr &expr) override
  {
    auto stmt = CompileConditionalBlocks::compile (&expr, ctx, nullptr);
    ctx->add_statement (stmt);
  }

  void visit (HIR::IfExprConseqElse &expr) override
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
	translated
	  = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
      }
  }

  void visit (HIR::IfExprConseqIf &expr) override
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
	translated
	  = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
      }
  }

  void visit (HIR::BlockExpr &expr) override
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
	translated
	  = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
      }
  }

  void visit (HIR::UnsafeBlockExpr &expr) override
  {
    expr.get_block_expr ()->accept_vis (*this);
  }

  void visit (HIR::StructExprStruct &struct_expr) override
  {
    TyTy::BaseType *tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (
	  struct_expr.get_mappings ().get_hirid (), &tyty))
      {
	rust_error_at (struct_expr.get_locus (), "unknown type");
	return;
      }

    rust_assert (tyty->is_unit ());
    translated = ctx->get_backend ()->unit_expression ();
  }

  void visit (HIR::StructExprStructFields &struct_expr) override
  {
    TyTy::BaseType *tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (
	  struct_expr.get_mappings ().get_hirid (), &tyty))
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

	ok = adt->lookup_variant_by_id (variant_id, &variant,
					&union_disriminator);
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
	auto respective_field
	  = variant->get_field_at_index (union_disriminator);
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
	    rvalue = coercion_site (rvalue, actual, expected, lvalue_locus,
				    rvalue_locus);
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
	    auto rvalue
	      = CompileStructExprField::Compile (argument.get (), ctx);

	    TyTy::BaseType *actual = nullptr;
	    bool ok = ctx->get_tyctx ()->lookup_type (
	      argument->get_mappings ().get_hirid (), &actual);

	    // coerce it if required/possible see
	    // compile/torture/struct_base_init_1.rs
	    if (ok)
	      {
		rvalue = coercion_site (rvalue, actual, expected, lvalue_locus,
					rvalue_locus);
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

  void visit (HIR::GroupedExpr &expr) override
  {
    translated = CompileExpr::Compile (expr.get_expr_in_parens ().get (), ctx);
  }

  void visit (HIR::FieldAccessExpr &expr) override
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
	bool ok = variant->lookup_field (expr.get_field_name (), nullptr,
					 &field_index);
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
	bool ok = variant->lookup_field (expr.get_field_name (), nullptr,
					 &field_index);
	rust_assert (ok);

	tree adt_tyty = TyTyResolveCompile::compile (ctx, adt);
	tree indirect
	  = ctx->get_backend ()->indirect_expression (adt_tyty, receiver_ref,
						      true, expr.get_locus ());
	receiver_ref = indirect;
      }

    translated
      = ctx->get_backend ()->struct_field_expression (receiver_ref, field_index,
						      expr.get_locus ());
  }

  void visit (HIR::QualifiedPathInExpression &expr) override
  {
    translated = ResolvePathRef::Compile (expr, ctx);
  }

  void visit (HIR::PathInExpression &expr) override
  {
    translated = ResolvePathRef::Compile (expr, ctx);
  }

  void visit (HIR::LoopExpr &expr) override
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
	tree label_decl
	  = ctx->get_backend ()->label_definition_statement (label);
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

  void visit (HIR::WhileLoopExpr &expr) override
  {
    fncontext fnctx = ctx->peek_fn ();
    if (expr.has_loop_label ())
      {
	HIR::LoopLabel &loop_label = expr.get_loop_label ();
	tree label
	  = ctx->get_backend ()->label (fnctx.fndecl,
					loop_label.get_lifetime ().get_name (),
					loop_label.get_locus ());
	tree label_decl
	  = ctx->get_backend ()->label_definition_statement (label);
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
    tree exit_expr
      = ctx->get_backend ()->exit_expression (condition, expr.get_locus ());
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

  void visit (HIR::BreakExpr &expr) override
  {
    if (expr.has_break_expr ())
      {
	tree compiled_expr
	  = CompileExpr::Compile (expr.get_expr ().get (), ctx);

	Bvariable *loop_result_holder = ctx->peek_loop_context ();
	tree result_reference = ctx->get_backend ()->var_expression (
	  loop_result_holder, expr.get_expr ()->get_locus ());

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
	      expr.get_label ().get_mappings ().get_nodeid (),
	      &resolved_node_id))
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
	    rust_fatal_error (expr.get_locus (),
			      "reverse lookup label failure");
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

  void visit (HIR::ContinueExpr &expr) override
  {
    tree label = ctx->peek_loop_begin_label ();
    if (expr.has_label ())
      {
	NodeId resolved_node_id = UNKNOWN_NODEID;
	if (!ctx->get_resolver ()->lookup_resolved_label (
	      expr.get_label ().get_mappings ().get_nodeid (),
	      &resolved_node_id))
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
	    rust_fatal_error (expr.get_locus (),
			      "reverse lookup label failure");
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

  void visit (HIR::BorrowExpr &expr) override;
  void visit (HIR::DereferenceExpr &expr) override;
  void visit (HIR::MatchExpr &expr) override;

  void visit (HIR::RangeFromToExpr &expr) override;

  void visit (HIR::RangeFromExpr &expr) override;

  void visit (HIR::RangeToExpr &expr) override;

  void visit (HIR::RangeFullExpr &expr) override;

  void visit (HIR::RangeFromToInclExpr &expr) override;

  // Empty visit for unused Expression HIR nodes.
  void visit (HIR::ClosureExprInner &) override {}
  void visit (HIR::ClosureExprInnerTyped &) override {}
  void visit (HIR::StructExprFieldIdentifier &) override {}
  void visit (HIR::StructExprFieldIdentifierValue &) override {}
  void visit (HIR::StructExprFieldIndexValue &) override {}
  void visit (HIR::ErrorPropagationExpr &) override {}
  void visit (HIR::RangeToInclExpr &) override {}
  void visit (HIR::WhileLetLoopExpr &) override {}
  void visit (HIR::ForLoopExpr &) override {}
  void visit (HIR::IfExprConseqIfLet &) override {}
  void visit (HIR::IfLetExpr &) override {}
  void visit (HIR::IfLetExprConseqElse &) override {}
  void visit (HIR::IfLetExprConseqIf &) override {}
  void visit (HIR::IfLetExprConseqIfLet &) override {}
  void visit (HIR::AwaitExpr &) override {}
  void visit (HIR::AsyncBlockExpr &) override {}

protected:
  tree get_fn_addr_from_dyn (const TyTy::DynamicObjectType *dyn,
			     TyTy::BaseType *receiver, TyTy::FnType *fntype,
			     tree receiver_ref, Location expr_locus);

  tree get_receiver_from_dyn (const TyTy::DynamicObjectType *dyn,
			      TyTy::BaseType *receiver, TyTy::FnType *fntype,
			      tree receiver_ref, Location expr_locus);

  tree resolve_method_address (TyTy::FnType *fntype, HirId ref,
			       TyTy::BaseType *receiver,
			       HIR::PathIdentSegment &segment,
			       Analysis::NodeMapping expr_mappings,
			       Location expr_locus);

  tree
  resolve_operator_overload (Analysis::RustLangItem::ItemType lang_item_type,
			     HIR::OperatorExprMeta expr, tree lhs, tree rhs,
			     HIR::Expr *lhs_expr, HIR::Expr *rhs_expr);

  tree compile_bool_literal (const HIR::LiteralExpr &expr,
			     const TyTy::BaseType *tyty);

  tree compile_integer_literal (const HIR::LiteralExpr &expr,
				const TyTy::BaseType *tyty);

  tree compile_float_literal (const HIR::LiteralExpr &expr,
			      const TyTy::BaseType *tyty);

  tree compile_char_literal (const HIR::LiteralExpr &expr,
			     const TyTy::BaseType *tyty);

  tree compile_byte_literal (const HIR::LiteralExpr &expr,
			     const TyTy::BaseType *tyty);

  tree compile_string_literal (const HIR::LiteralExpr &expr,
			       const TyTy::BaseType *tyty);

  tree compile_byte_string_literal (const HIR::LiteralExpr &expr,
				    const TyTy::BaseType *tyty);

  tree type_cast_expression (tree type_to_cast_to, tree expr, Location locus);

  tree array_value_expr (Location expr_locus, const TyTy::ArrayType &array_tyty,
			 tree array_type, HIR::ArrayElemsValues &elems);

  tree array_copied_expr (Location expr_locus,
			  const TyTy::ArrayType &array_tyty, tree array_type,
			  HIR::ArrayElemsCopied &elems);

private:
  CompileExpr (Context *ctx)
    : HIRCompileBase (ctx), translated (error_mark_node)
  {}

  tree translated;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_EXPR
