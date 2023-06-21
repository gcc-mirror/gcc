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

#include "rust-compile-pattern.h"
#include "rust-compile-expr.h"
#include "rust-compile-resolve-path.h"
#include "rust-constexpr.h"

namespace Rust {
namespace Compile {

void
CompilePatternCaseLabelExpr::visit (HIR::PathInExpression &pattern)
{
  // lookup the type
  TyTy::BaseType *lookup = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (pattern.get_mappings ().get_hirid (),
				      &lookup);
  rust_assert (ok);

  // this must be an enum
  rust_assert (lookup->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (lookup);
  rust_assert (adt->is_enum ());

  // lookup the variant
  HirId variant_id;
  ok = ctx->get_tyctx ()->lookup_variant_definition (
    pattern.get_mappings ().get_hirid (), &variant_id);
  rust_assert (ok);

  TyTy::VariantDef *variant = nullptr;
  ok = adt->lookup_variant_by_id (variant_id, &variant);
  rust_assert (ok);

  HIR::Expr *discrim_expr = variant->get_discriminant ();
  tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);
  tree folded_discrim_expr = fold_expr (discrim_expr_node);
  tree case_low = folded_discrim_expr;

  case_label_expr
    = build_case_label (case_low, NULL_TREE, associated_case_label);
}

void
CompilePatternCaseLabelExpr::visit (HIR::StructPattern &pattern)
{
  CompilePatternCaseLabelExpr::visit (pattern.get_path ());
}

void
CompilePatternCaseLabelExpr::visit (HIR::TupleStructPattern &pattern)
{
  CompilePatternCaseLabelExpr::visit (pattern.get_path ());
}

void
CompilePatternCaseLabelExpr::visit (HIR::WildcardPattern &pattern)
{
  // operand 0 being NULL_TREE signifies this is the default case label see:
  // tree.def for documentation for CASE_LABEL_EXPR
  case_label_expr
    = build_case_label (NULL_TREE, NULL_TREE, associated_case_label);
}

void
CompilePatternCaseLabelExpr::visit (HIR::LiteralPattern &pattern)
{
  // Compile the literal
  HIR::LiteralExpr *litexpr
    = new HIR::LiteralExpr (pattern.get_pattern_mappings (),
			    pattern.get_literal (), pattern.get_locus (),
			    std::vector<AST::Attribute> ());

  // Note: Floating point literals are currently accepted but will likely be
  // forbidden in LiteralPatterns in a future version of Rust.
  // See: https://github.com/rust-lang/rust/issues/41620
  // For now, we cannot compile them anyway as CASE_LABEL_EXPR does not support
  // floating point types.
  if (pattern.get_literal ().get_lit_type () == HIR::Literal::LitType::FLOAT)
    {
      rust_sorry_at (pattern.get_locus (), "floating-point literal in pattern");
    }

  tree lit = CompileExpr::Compile (litexpr, ctx);

  case_label_expr = build_case_label (lit, NULL_TREE, associated_case_label);
}

static tree
compile_range_pattern_bound (HIR::RangePatternBound *bound,
			     Analysis::NodeMapping mappings, Location locus,
			     Context *ctx)
{
  tree result = NULL_TREE;
  switch (bound->get_bound_type ())
    {
      case HIR::RangePatternBound::RangePatternBoundType::LITERAL: {
	HIR::RangePatternBoundLiteral &ref
	  = *static_cast<HIR::RangePatternBoundLiteral *> (bound);

	HIR::LiteralExpr *litexpr
	  = new HIR::LiteralExpr (mappings, ref.get_literal (), locus,
				  std::vector<AST::Attribute> ());

	result = CompileExpr::Compile (litexpr, ctx);
      }
      break;

      case HIR::RangePatternBound::RangePatternBoundType::PATH: {
	HIR::RangePatternBoundPath &ref
	  = *static_cast<HIR::RangePatternBoundPath *> (bound);

	result = ResolvePathRef::Compile (ref.get_path (), ctx);

	// If the path resolves to a const expression, fold it.
	result = fold_expr (result);
      }
      break;

      case HIR::RangePatternBound::RangePatternBoundType::QUALPATH: {
	HIR::RangePatternBoundQualPath &ref
	  = *static_cast<HIR::RangePatternBoundQualPath *> (bound);

	result = ResolvePathRef::Compile (ref.get_qualified_path (), ctx);

	// If the path resolves to a const expression, fold it.
	result = fold_expr (result);
      }
    }

  return result;
}

void
CompilePatternCaseLabelExpr::visit (HIR::RangePattern &pattern)
{
  tree upper = compile_range_pattern_bound (pattern.get_upper_bound ().get (),
					    pattern.get_pattern_mappings (),
					    pattern.get_locus (), ctx);
  tree lower = compile_range_pattern_bound (pattern.get_lower_bound ().get (),
					    pattern.get_pattern_mappings (),
					    pattern.get_locus (), ctx);

  case_label_expr = build_case_label (lower, upper, associated_case_label);
}

// setup the bindings

void
CompilePatternBindings::visit (HIR::TupleStructPattern &pattern)
{
  // lookup the type
  TyTy::BaseType *lookup = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    pattern.get_path ().get_mappings ().get_hirid (), &lookup);
  rust_assert (ok);

  // this must be an enum
  rust_assert (lookup->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (lookup);
  rust_assert (adt->number_of_variants () > 0);

  int variant_index = 0;
  TyTy::VariantDef *variant = adt->get_variants ().at (0);
  if (adt->is_enum ())
    {
      HirId variant_id = UNKNOWN_HIRID;
      bool ok = ctx->get_tyctx ()->lookup_variant_definition (
	pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
      rust_assert (ok);

      ok = adt->lookup_variant_by_id (variant_id, &variant, &variant_index);
      rust_assert (ok);
    }

  rust_assert (variant->get_variant_type ()
	       == TyTy::VariantDef::VariantType::TUPLE);

  std::unique_ptr<HIR::TupleStructItems> &items = pattern.get_items ();
  switch (items->get_item_type ())
    {
      case HIR::TupleStructItems::RANGE: {
	// TODO
	gcc_unreachable ();
      }
      break;

      case HIR::TupleStructItems::NO_RANGE: {
	HIR::TupleStructItemsNoRange &items_no_range
	  = static_cast<HIR::TupleStructItemsNoRange &> (*items.get ());

	rust_assert (items_no_range.get_patterns ().size ()
		     == variant->num_fields ());

	if (adt->is_enum ())
	  {
	    // we are offsetting by + 1 here since the first field in the record
	    // is always the discriminator
	    size_t tuple_field_index = 1;
	    for (auto &pattern : items_no_range.get_patterns ())
	      {
		tree variant_accessor
		  = ctx->get_backend ()->struct_field_expression (
		    match_scrutinee_expr, variant_index, pattern->get_locus ());

		tree binding = ctx->get_backend ()->struct_field_expression (
		  variant_accessor, tuple_field_index++, pattern->get_locus ());

		ctx->insert_pattern_binding (
		  pattern->get_pattern_mappings ().get_hirid (), binding);
	      }
	  }
	else
	  {
	    size_t tuple_field_index = 0;
	    for (auto &pattern : items_no_range.get_patterns ())
	      {
		tree variant_accessor = match_scrutinee_expr;

		tree binding = ctx->get_backend ()->struct_field_expression (
		  variant_accessor, tuple_field_index++, pattern->get_locus ());

		ctx->insert_pattern_binding (
		  pattern->get_pattern_mappings ().get_hirid (), binding);
	      }
	  }
      }
      break;
    }
}

void
CompilePatternBindings::visit (HIR::StructPattern &pattern)
{
  // lookup the type
  TyTy::BaseType *lookup = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    pattern.get_path ().get_mappings ().get_hirid (), &lookup);
  rust_assert (ok);

  // this must be an enum
  rust_assert (lookup->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (lookup);
  rust_assert (adt->number_of_variants () > 0);

  int variant_index = 0;
  TyTy::VariantDef *variant = adt->get_variants ().at (0);
  if (adt->is_enum ())
    {
      HirId variant_id = UNKNOWN_HIRID;
      bool ok = ctx->get_tyctx ()->lookup_variant_definition (
	pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
      rust_assert (ok);

      ok = adt->lookup_variant_by_id (variant_id, &variant, &variant_index);
      rust_assert (ok);
    }

  rust_assert (variant->get_variant_type ()
	       == TyTy::VariantDef::VariantType::STRUCT);

  auto &struct_pattern_elems = pattern.get_struct_pattern_elems ();
  for (auto &field : struct_pattern_elems.get_struct_pattern_fields ())
    {
      switch (field->get_item_type ())
	{
	  case HIR::StructPatternField::ItemType::TUPLE_PAT: {
	    // TODO
	    gcc_unreachable ();
	  }
	  break;

	  case HIR::StructPatternField::ItemType::IDENT_PAT: {
	    // TODO
	    gcc_unreachable ();
	  }
	  break;

	  case HIR::StructPatternField::ItemType::IDENT: {
	    HIR::StructPatternFieldIdent &ident
	      = static_cast<HIR::StructPatternFieldIdent &> (*field.get ());

	    size_t offs = 0;
	    ok
	      = variant->lookup_field (ident.get_identifier (), nullptr, &offs);
	    rust_assert (ok);

	    tree binding = error_mark_node;
	    if (adt->is_enum ())
	      {
		tree variant_accessor
		  = ctx->get_backend ()->struct_field_expression (
		    match_scrutinee_expr, variant_index, ident.get_locus ());

		// we are offsetting by + 1 here since the first field in the
		// record is always the discriminator
		binding = ctx->get_backend ()->struct_field_expression (
		  variant_accessor, offs + 1, ident.get_locus ());
	      }
	    else
	      {
		tree variant_accessor = match_scrutinee_expr;
		binding = ctx->get_backend ()->struct_field_expression (
		  variant_accessor, offs, ident.get_locus ());
	      }

	    ctx->insert_pattern_binding (ident.get_mappings ().get_hirid (),
					 binding);
	  }
	  break;
	}
    }
}

void
CompilePatternLet::visit (HIR::IdentifierPattern &pattern)
{
  Bvariable *var = nullptr;
  rust_assert (
    ctx->lookup_var_decl (pattern.get_pattern_mappings ().get_hirid (), &var));

  auto fnctx = ctx->peek_fn ();
  if (ty->is_unit ())
    {
      ctx->add_statement (init_expr);

      tree stmt_type = TyTyResolveCompile::compile (ctx, ty);

      auto unit_type_init_expr
	= ctx->get_backend ()->constructor_expression (stmt_type, false, {}, -1,
						       rval_locus);
      auto s = ctx->get_backend ()->init_statement (fnctx.fndecl, var,
						    unit_type_init_expr);
      ctx->add_statement (s);
    }
  else
    {
      auto s
	= ctx->get_backend ()->init_statement (fnctx.fndecl, var, init_expr);
      ctx->add_statement (s);
    }
}

void
CompilePatternLet::visit (HIR::WildcardPattern &pattern)
{
  tree init_stmt = NULL;
  tree stmt_type = TyTyResolveCompile::compile (ctx, ty);

  ctx->get_backend ()->temporary_variable (ctx->peek_fn ().fndecl, NULL_TREE,
					   stmt_type, init_expr, false,
					   pattern.get_locus (), &init_stmt);

  ctx->add_statement (init_stmt);
}

void
CompilePatternLet::visit (HIR::TuplePattern &pattern)
{
  rust_assert (pattern.has_tuple_pattern_items ());

  tree tuple_type = TyTyResolveCompile::compile (ctx, ty);
  tree init_stmt;
  Bvariable *tmp_var
    = ctx->get_backend ()->temporary_variable (ctx->peek_fn ().fndecl,
					       NULL_TREE, tuple_type, init_expr,
					       false, pattern.get_locus (),
					       &init_stmt);
  tree access_expr
    = ctx->get_backend ()->var_expression (tmp_var, pattern.get_locus ());
  ctx->add_statement (init_stmt);

  switch (pattern.get_items ()->get_pattern_type ())
    {
      case HIR::TuplePatternItems::TuplePatternItemType::RANGED: {
	size_t tuple_idx = 0;
	auto &items
	  = static_cast<HIR::TuplePatternItemsRanged &> (*pattern.get_items ());

	auto &items_lower = items.get_lower_patterns ();
	auto &items_upper = items.get_upper_patterns ();

	for (auto &sub : items_lower)
	  {
	    TyTy::BaseType *ty_sub = nullptr;
	    HirId pattern_id = pattern.get_pattern_mappings ().get_hirid ();
	    bool ok = ctx->get_tyctx ()->lookup_type (pattern_id, &ty_sub);
	    rust_assert (ok);

	    tree sub_init = ctx->get_backend ()->struct_field_expression (
	      access_expr, tuple_idx, sub->get_locus ());
	    CompilePatternLet::Compile (sub.get (), sub_init, ty_sub,
					rval_locus, ctx);
	    tuple_idx++;
	  }

	rust_assert (ty->get_kind () == TyTy::TypeKind::TUPLE);
	tuple_idx = static_cast<TyTy::TupleType &> (*ty).num_fields ()
		    - items_upper.size ();

	for (auto &sub : items_upper)
	  {
	    TyTy::BaseType *ty_sub = nullptr;
	    HirId pattern_id = pattern.get_pattern_mappings ().get_hirid ();
	    bool ok = ctx->get_tyctx ()->lookup_type (pattern_id, &ty_sub);
	    rust_assert (ok);

	    tree sub_init = ctx->get_backend ()->struct_field_expression (
	      access_expr, tuple_idx, sub->get_locus ());
	    CompilePatternLet::Compile (sub.get (), sub_init, ty_sub,
					rval_locus, ctx);
	    tuple_idx++;
	  }

	return;
      }
      case HIR::TuplePatternItems::TuplePatternItemType::MULTIPLE: {
	size_t tuple_idx = 0;
	auto &items = static_cast<HIR::TuplePatternItemsMultiple &> (
	  *pattern.get_items ());

	for (auto &sub : items.get_patterns ())
	  {
	    TyTy::BaseType *ty_sub = nullptr;
	    HirId pattern_id = pattern.get_pattern_mappings ().get_hirid ();
	    bool ok = ctx->get_tyctx ()->lookup_type (pattern_id, &ty_sub);
	    rust_assert (ok);

	    tree sub_init = ctx->get_backend ()->struct_field_expression (
	      access_expr, tuple_idx, sub->get_locus ());
	    CompilePatternLet::Compile (sub.get (), sub_init, ty_sub,
					rval_locus, ctx);
	    tuple_idx++;
	  }

	return;
      }
      default: {
	gcc_unreachable ();
      }
    }
}

} // namespace Compile
} // namespace Rust
