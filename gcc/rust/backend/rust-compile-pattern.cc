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

#include "rust-compile-pattern.h"
#include "rust-compile-expr.h"
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
  tree folded_discrim_expr = ConstCtx::fold (discrim_expr_node);
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
  rust_assert (adt->is_enum ());

  // lookup the variant
  HirId variant_id;
  ok = ctx->get_tyctx ()->lookup_variant_definition (
    pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
  rust_assert (ok);

  int variant_index = -1;
  TyTy::VariantDef *variant = nullptr;
  ok = adt->lookup_variant_by_id (variant_id, &variant, &variant_index);
  rust_assert (ok);

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
  rust_assert (adt->is_enum ());

  // lookup the variant
  HirId variant_id;
  ok = ctx->get_tyctx ()->lookup_variant_definition (
    pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
  rust_assert (ok);

  int variant_index = -1;
  TyTy::VariantDef *variant = nullptr;
  ok = adt->lookup_variant_by_id (variant_id, &variant, &variant_index);
  rust_assert (ok);

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

	    tree variant_accessor
	      = ctx->get_backend ()->struct_field_expression (
		match_scrutinee_expr, variant_index, ident.get_locus ());

	    size_t offs = 0;
	    ok
	      = variant->lookup_field (ident.get_identifier (), nullptr, &offs);
	    rust_assert (ok);

	    // we are offsetting by + 1 here since the first field in the record
	    // is always the discriminator
	    tree binding = ctx->get_backend ()->struct_field_expression (
	      variant_accessor, offs + 1, ident.get_locus ());

	    ctx->insert_pattern_binding (ident.get_mappings ().get_hirid (),
					 binding);
	  }
	  break;
	}
    }
}

} // namespace Compile
} // namespace Rust
