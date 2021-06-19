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

#ifndef RUST_COMPILE_EXPR
#define RUST_COMPILE_EXPR

#include "rust-compile-base.h"
#include "rust-compile-tyty.h"
#include "rust-compile-resolve-path.h"
#include "rust-compile-block.h"
#include "rust-compile-struct-field-expr.h"

namespace Rust {
namespace Compile {

class CompileExpr : public HIRCompileBase
{
  using Rust::Compile::HIRCompileBase::visit;

public:
  static Bexpression *Compile (HIR::Expr *expr, Context *ctx)
  {
    CompileExpr compiler (ctx);
    expr->accept_vis (compiler);
    return compiler.translated;
  }

  void visit (HIR::TupleIndexExpr &expr) override
  {
    HIR::Expr *tuple_expr = expr.get_tuple_expr ().get ();
    TupleIndex index = expr.get_tuple_index ();

    Bexpression *receiver_ref = CompileExpr::Compile (tuple_expr, ctx);
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

    Btype *tuple_type = TyTyResolveCompile::compile (ctx, tyty);
    rust_assert (tuple_type != nullptr);

    // this assumes all fields are in order from type resolution
    std::vector<Bexpression *> vals;
    for (auto &elem : expr.get_tuple_elems ())
      {
	auto e = CompileExpr::Compile (elem.get (), ctx);
	vals.push_back (e);
      }

    translated
      = ctx->get_backend ()->constructor_expression (tuple_type, vals,
						     expr.get_locus ());
  }

  void visit (HIR::ReturnExpr &expr) override
  {
    auto fncontext = ctx->peek_fn ();

    std::vector<Bexpression *> retstmts;
    if (expr.has_return_expr ())
      {
	Bexpression *compiled_expr
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

  void visit (HIR::IdentifierExpr &expr) override
  {
    // need to look up the reference for this identifier
    NodeId ref_node_id;
    if (!ctx->get_resolver ()->lookup_resolved_name (
	  expr.get_mappings ().get_nodeid (), &ref_node_id))
      {
	rust_fatal_error (expr.get_locus (), "failed to look up resolved name");
	return;
      }

    // these ref_node_ids will resolve to a pattern declaration but we are
    // interested in the definition that this refers to get the parent id
    Resolver::Definition def;
    if (!ctx->get_resolver ()->lookup_definition (ref_node_id, &def))
      {
	rust_error_at (expr.get_locus (), "unknown reference");
	return;
      }

    HirId ref;
    if (!ctx->get_mappings ()->lookup_node_to_hir (
	  expr.get_mappings ().get_crate_num (), def.parent, &ref))
      {
	rust_fatal_error (expr.get_locus (), "reverse lookup failure");
	return;
      }

    Bfunction *fn = nullptr;
    Bvariable *var = nullptr;
    if (ctx->lookup_const_decl (ref, &translated))
      {
	return;
      }
    else if (ctx->lookup_function_decl (ref, &fn))
      {
	translated
	  = ctx->get_backend ()->function_code_expression (fn,
							   expr.get_locus ());
      }
    else if (ctx->lookup_var_decl (ref, &var))
      {
	translated
	  = ctx->get_backend ()->var_expression (var, expr.get_locus ());
      }
    else
      {
	rust_fatal_error (expr.get_locus (),
			  "failed to lookup compiled reference");
      }
  }

  void visit (HIR::LiteralExpr &expr) override
  {
    auto literal_value = expr.get_literal ();
    switch (expr.get_lit_type ())
      {
	case HIR::Literal::BOOL: {
	  bool bval = literal_value->as_string ().compare ("true") == 0;
	  translated = ctx->get_backend ()->boolean_constant_expression (bval);
	}
	return;

	case HIR::Literal::INT: {
	  mpz_t ival;
	  if (mpz_init_set_str (ival, literal_value->as_string ().c_str (), 10)
	      != 0)
	    {
	      rust_fatal_error (expr.get_locus (), "bad number in literal");
	      return;
	    }

	  TyTy::BaseType *tyty = nullptr;
	  if (!ctx->get_tyctx ()->lookup_type (
		expr.get_mappings ().get_hirid (), &tyty))
	    {
	      rust_fatal_error (
		expr.get_locus (),
		"did not resolve type for this literal expr (HirId %d)",
		expr.get_mappings ().get_hirid ());
	      return;
	    }

	  Btype *type = TyTyResolveCompile::compile (ctx, tyty);
	  translated
	    = ctx->get_backend ()->integer_constant_expression (type, ival);
	}
	return;

	case HIR::Literal::FLOAT: {
	  mpfr_t fval;
	  if (mpfr_init_set_str (fval, literal_value->as_string ().c_str (), 10,
				 MPFR_RNDN)
	      != 0)
	    {
	      rust_fatal_error (expr.get_locus (),
				"bad floating-point number in literal");
	      return;
	    }

	  TyTy::BaseType *tyty = nullptr;
	  if (!ctx->get_tyctx ()->lookup_type (
		expr.get_mappings ().get_hirid (), &tyty))
	    {
	      rust_fatal_error (expr.get_locus (),
				"did not resolve type for this literal expr");
	      return;
	    }

	  Btype *type = TyTyResolveCompile::compile (ctx, tyty);
	  translated
	    = ctx->get_backend ()->float_constant_expression (type, fval);
	}
	return;

	case HIR::Literal::CHAR: {
	  // FIXME needs wchar_t
	  char c = literal_value->as_string ().c_str ()[0];
	  translated = ctx->get_backend ()->wchar_constant_expression (c);
	}
	return;

	case HIR::Literal::STRING: {
	  auto base = ctx->get_backend ()->string_constant_expression (
	    literal_value->as_string ());
	  translated
	    = ctx->get_backend ()->address_expression (base, expr.get_locus ());
	}
	return;

      default:
	rust_fatal_error (expr.get_locus (), "unknown literal");
	return;
      }

    gcc_unreachable ();
  }

  void visit (HIR::AssignmentExpr &expr) override
  {
    fncontext fn = ctx->peek_fn ();
    auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);

    Bstatement *assignment
      = ctx->get_backend ()->assignment_statement (fn.fndecl, lhs, rhs,
						   expr.get_locus ());
    ctx->add_statement (assignment);
  }

  void visit (HIR::ArrayIndexExpr &expr) override
  {
    Bexpression *array = CompileExpr::Compile (expr.get_array_expr (), ctx);
    Bexpression *index = CompileExpr::Compile (expr.get_index_expr (), ctx);
    translated
      = ctx->get_backend ()->array_index_expression (array, index,
						     expr.get_locus ());
  }

  void visit (HIR::ArrayExpr &expr) override
  {
    TyTy::BaseType *tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &tyty))
      {
	rust_fatal_error (expr.get_locus (),
			  "did not resolve type for this array expr");
	return;
      }

    rust_assert (tyty->get_kind () == TyTy::TypeKind::ARRAY);
    TyTy::ArrayType *array_tyty = static_cast<TyTy::ArrayType *> (tyty);
    capacity_expr = array_tyty->get_capacity ();

    Btype *array_type = TyTyResolveCompile::compile (ctx, array_tyty);
    rust_assert (array_type != nullptr);

    expr.get_internal_elements ()->accept_vis (*this);
    std::vector<unsigned long> indexes;
    for (size_t i = 0; i < constructor.size (); i++)
      indexes.push_back (i);

    translated
      = ctx->get_backend ()->array_constructor_expression (array_type, indexes,
							   constructor,
							   expr.get_locus ());
  }

  void visit (HIR::ArrayElemsValues &elems) override
  {
    elems.iterate ([&] (HIR::Expr *e) mutable -> bool {
      Bexpression *translated_expr = CompileExpr::Compile (e, ctx);
      constructor.push_back (translated_expr);
      return true;
    });
  }

  void visit (HIR::ArrayElemsCopied &elems) override
  {
    Bexpression *translated_expr
      = CompileExpr::Compile (elems.get_elem_to_copy (), ctx);

    size_t capacity;
    bool ok = ctx->get_backend ()->const_size_cast (capacity_expr, &capacity);
    rust_assert (ok);

    for (size_t i = 0; i < capacity; ++i)
      constructor.push_back (translated_expr);
  }

  void visit (HIR::ArithmeticOrLogicalExpr &expr) override
  {
    auto op = expr.get_expr_type ();
    auto lhs = CompileExpr::Compile (expr.get_lhs (), ctx);
    auto rhs = CompileExpr::Compile (expr.get_rhs (), ctx);
    auto location = expr.get_locus ();

    translated
      = ctx->get_backend ()->arithmetic_or_logical_expression (op, lhs, rhs,
							       location);
  }

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

  void visit (HIR::NegationExpr &expr) override
  {
    auto op = expr.get_expr_type ();
    auto negated_expr = CompileExpr::Compile (expr.get_expr (), ctx);
    auto location = expr.get_locus ();

    translated
      = ctx->get_backend ()->negation_expression (op, negated_expr, location);
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
	Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
	Btype *block_type = TyTyResolveCompile::compile (ctx, if_type);

	bool is_address_taken = false;
	Bstatement *ret_var_stmt = nullptr;
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
	Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
	Btype *block_type = TyTyResolveCompile::compile (ctx, if_type);

	bool is_address_taken = false;
	Bstatement *ret_var_stmt = nullptr;
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
	Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
	Btype *block_type = TyTyResolveCompile::compile (ctx, block_tyty);

	bool is_address_taken = false;
	Bstatement *ret_var_stmt = nullptr;
	tmp = ctx->get_backend ()->temporary_variable (
	  fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	  expr.get_locus (), &ret_var_stmt);
	ctx->add_statement (ret_var_stmt);
      }

    auto code_block = CompileBlock::compile (&expr, ctx, tmp);
    auto block_stmt = ctx->get_backend ()->block_statement (code_block);
    ctx->add_statement (block_stmt);

    if (tmp != NULL)
      {
	translated
	  = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
      }
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

    Btype *type = TyTyResolveCompile::compile (ctx, tyty);
    rust_assert (type != nullptr);

    // this assumes all fields are in order from type resolution and if a base
    // struct was specified those fields are filed via accesors
    std::vector<Bexpression *> vals;
    struct_expr.iterate ([&] (HIR::StructExprField *field) mutable -> bool {
      Bexpression *expr = CompileStructExprField::Compile (field, ctx);
      vals.push_back (expr);
      return true;
    });

    translated
      = ctx->get_backend ()->constructor_expression (type, vals,
						     struct_expr.get_locus ());
  }

  void visit (HIR::GroupedExpr &expr) override
  {
    translated = CompileExpr::Compile (expr.get_expr_in_parens ().get (), ctx);
  }

  void visit (HIR::FieldAccessExpr &expr) override
  {
    // resolve the receiver back to ADT type
    TyTy::BaseType *receiver = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (
	  expr.get_receiver_expr ()->get_mappings ().get_hirid (), &receiver))
      {
	rust_error_at (expr.get_receiver_expr ()->get_locus_slow (),
		       "unresolved type for receiver");
	return;
      }
    rust_assert (receiver->get_kind () == TyTy::TypeKind::ADT);
    TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (receiver);

    size_t index = 0;
    adt->get_field (expr.get_field_name (), &index);

    Bexpression *struct_ref
      = CompileExpr::Compile (expr.get_receiver_expr ().get (), ctx);

    translated
      = ctx->get_backend ()->struct_field_expression (struct_ref, index,
						      expr.get_locus ());
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
    Bvariable *tmp = NULL;
    bool needs_temp = !block_tyty->is_unit ();
    if (needs_temp)
      {
	Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
	Btype *block_type = TyTyResolveCompile::compile (ctx, block_tyty);

	bool is_address_taken = false;
	Bstatement *ret_var_stmt = nullptr;
	tmp = ctx->get_backend ()->temporary_variable (
	  fnctx.fndecl, enclosing_scope, block_type, NULL, is_address_taken,
	  expr.get_locus (), &ret_var_stmt);
	ctx->add_statement (ret_var_stmt);
	ctx->push_loop_context (tmp);
      }

    if (expr.has_loop_label ())
      {
	HIR::LoopLabel &loop_label = expr.get_loop_label ();
	Blabel *label
	  = ctx->get_backend ()->label (fnctx.fndecl,
					loop_label.get_lifetime ().get_name (),
					loop_label.get_locus ());
	Bstatement *label_decl
	  = ctx->get_backend ()->label_definition_statement (label);
	ctx->add_statement (label_decl);
	ctx->insert_label_decl (
	  loop_label.get_lifetime ().get_mappings ().get_hirid (), label);
      }

    Blabel *loop_begin_label
      = ctx->get_backend ()->label (fnctx.fndecl, "", expr.get_locus ());
    Bstatement *loop_begin_label_decl
      = ctx->get_backend ()->label_definition_statement (loop_begin_label);
    ctx->add_statement (loop_begin_label_decl);
    ctx->push_loop_begin_label (loop_begin_label);

    Bblock *code_block
      = CompileBlock::compile (expr.get_loop_block ().get (), ctx, nullptr);
    Bexpression *loop_expr
      = ctx->get_backend ()->loop_expression (code_block, expr.get_locus ());
    Bstatement *loop_stmt
      = ctx->get_backend ()->expression_statement (fnctx.fndecl, loop_expr);
    ctx->add_statement (loop_stmt);

    if (tmp != NULL)
      {
	ctx->pop_loop_context ();
	translated
	  = ctx->get_backend ()->var_expression (tmp, expr.get_locus ());
      }
    ctx->pop_loop_begin_label ();
  }

  void visit (HIR::WhileLoopExpr &expr) override
  {
    fncontext fnctx = ctx->peek_fn ();
    if (expr.has_loop_label ())
      {
	HIR::LoopLabel &loop_label = expr.get_loop_label ();
	Blabel *label
	  = ctx->get_backend ()->label (fnctx.fndecl,
					loop_label.get_lifetime ().get_name (),
					loop_label.get_locus ());
	Bstatement *label_decl
	  = ctx->get_backend ()->label_definition_statement (label);
	ctx->add_statement (label_decl);
	ctx->insert_label_decl (
	  loop_label.get_lifetime ().get_mappings ().get_hirid (), label);
      }

    std::vector<Bvariable *> locals;
    Location start_location = expr.get_loop_block ()->get_locus ();
    Location end_location = expr.get_loop_block ()->get_locus (); // FIXME

    Bblock *enclosing_scope = ctx->peek_enclosing_scope ();
    Bblock *loop_block
      = ctx->get_backend ()->block (fnctx.fndecl, enclosing_scope, locals,
				    start_location, end_location);
    ctx->push_block (loop_block);

    Blabel *loop_begin_label
      = ctx->get_backend ()->label (fnctx.fndecl, "", expr.get_locus ());
    Bstatement *loop_begin_label_decl
      = ctx->get_backend ()->label_definition_statement (loop_begin_label);
    ctx->add_statement (loop_begin_label_decl);
    ctx->push_loop_begin_label (loop_begin_label);

    Bexpression *condition
      = CompileExpr::Compile (expr.get_predicate_expr ().get (), ctx);
    Bexpression *exit_expr
      = ctx->get_backend ()->exit_expression (condition, expr.get_locus ());
    Bstatement *break_stmt
      = ctx->get_backend ()->expression_statement (fnctx.fndecl, exit_expr);
    ctx->add_statement (break_stmt);

    Bblock *code_block
      = CompileBlock::compile (expr.get_loop_block ().get (), ctx, nullptr);
    Bstatement *code_block_stmt
      = ctx->get_backend ()->block_statement (code_block);
    ctx->add_statement (code_block_stmt);

    ctx->pop_loop_begin_label ();
    ctx->pop_block ();

    Bexpression *loop_expr
      = ctx->get_backend ()->loop_expression (loop_block, expr.get_locus ());
    Bstatement *loop_stmt
      = ctx->get_backend ()->expression_statement (fnctx.fndecl, loop_expr);
    ctx->add_statement (loop_stmt);
  }

  void visit (HIR::BreakExpr &expr) override
  {
    fncontext fnctx = ctx->peek_fn ();
    if (expr.has_break_expr ())
      {
	Bexpression *compiled_expr
	  = CompileExpr::Compile (expr.get_expr ().get (), ctx);

	Bvariable *loop_result_holder = ctx->peek_loop_context ();
	Bexpression *result_reference = ctx->get_backend ()->var_expression (
	  loop_result_holder, expr.get_expr ()->get_locus_slow ());

	Bstatement *assignment = ctx->get_backend ()->assignment_statement (
	  fnctx.fndecl, result_reference, compiled_expr, expr.get_locus ());
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
	if (!ctx->get_mappings ()->lookup_node_to_hir (
	      expr.get_mappings ().get_crate_num (), resolved_node_id, &ref))
	  {
	    rust_fatal_error (expr.get_locus (),
			      "reverse lookup label failure");
	    return;
	  }

	Blabel *label = nullptr;
	if (!ctx->lookup_label_decl (ref, &label))
	  {
	    rust_error_at (expr.get_label ().get_locus (),
			   "failed to lookup compiled label");
	    return;
	  }

	Bstatement *goto_label
	  = ctx->get_backend ()->goto_statement (label, expr.get_locus ());
	ctx->add_statement (goto_label);
      }
    else
      {
	Bexpression *exit_expr = ctx->get_backend ()->exit_expression (
	  ctx->get_backend ()->boolean_constant_expression (true),
	  expr.get_locus ());
	Bstatement *break_stmt
	  = ctx->get_backend ()->expression_statement (fnctx.fndecl, exit_expr);
	ctx->add_statement (break_stmt);
      }
  }

  void visit (HIR::ContinueExpr &expr) override
  {
    Blabel *label = ctx->peek_loop_begin_label ();
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
	if (!ctx->get_mappings ()->lookup_node_to_hir (
	      expr.get_mappings ().get_crate_num (), resolved_node_id, &ref))
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

    Bstatement *goto_label
      = ctx->get_backend ()->goto_statement (label, expr.get_locus ());
    ctx->add_statement (goto_label);
  }

  void visit (HIR::BorrowExpr &expr) override
  {
    Bexpression *main_expr
      = CompileExpr::Compile (expr.get_expr ().get (), ctx);

    translated
      = ctx->get_backend ()->address_expression (main_expr, expr.get_locus ());
  }

  void visit (HIR::DereferenceExpr &expr) override
  {
    Bexpression *main_expr
      = CompileExpr::Compile (expr.get_expr ().get (), ctx);

    TyTy::BaseType *tyty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (expr.get_mappings ().get_hirid (),
					 &tyty))
      {
	rust_fatal_error (expr.get_locus (),
			  "did not resolve type for this TupleExpr");
	return;
      }

    Btype *expected_type = TyTyResolveCompile::compile (ctx, tyty);
    bool known_valid = true;
    translated
      = ctx->get_backend ()->indirect_expression (expected_type, main_expr,
						  known_valid,
						  expr.get_locus ());
  }

private:
  CompileExpr (Context *ctx)
    : HIRCompileBase (ctx), translated (nullptr), capacity_expr (nullptr)
  {}

  Bexpression *translated;
  Bexpression *capacity_expr;
  std::vector<Bexpression *> constructor;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_EXPR
