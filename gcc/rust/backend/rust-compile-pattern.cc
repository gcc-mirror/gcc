// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
#include "rust-compile-type.h"

namespace Rust {
namespace Compile {

void
CompilePatternCheckExpr::visit (HIR::PathInExpression &pattern)
{
  // lookup the type
  TyTy::BaseType *lookup = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (pattern.get_mappings ().get_hirid (),
				      &lookup);
  rust_assert (ok);

  // this must be an enum
  // TODO: might not be
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

  // find discriminant field of scrutinee
  tree scrutinee_record_expr
    = Backend::struct_field_expression (match_scrutinee_expr, 0,
					pattern.get_locus ());
  tree scrutinee_expr_qualifier_expr
    = Backend::struct_field_expression (scrutinee_record_expr, 0,
					pattern.get_locus ());

  // must be enum
  match_scrutinee_expr = scrutinee_expr_qualifier_expr;

  HIR::Expr *discrim_expr = variant->get_discriminant ();
  tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);

  check_expr
    = Backend::comparison_expression (ComparisonOperator::EQUAL,
				      match_scrutinee_expr, discrim_expr_node,
				      pattern.get_locus ());
}

void
CompilePatternCheckExpr::visit (HIR::LiteralPattern &pattern)
{
  // Compile the literal
  HIR::LiteralExpr *litexpr
    = new HIR::LiteralExpr (pattern.get_mappings (), pattern.get_literal (),
			    pattern.get_locus (),
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

  check_expr = Backend::comparison_expression (ComparisonOperator::EQUAL,
					       match_scrutinee_expr, lit,
					       pattern.get_locus ());
}

static tree
compile_range_pattern_bound (HIR::RangePatternBound *bound,
			     Analysis::NodeMapping mappings, location_t locus,
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
CompilePatternCheckExpr::visit (HIR::RangePattern &pattern)
{
  tree upper = compile_range_pattern_bound (pattern.get_upper_bound ().get (),
					    pattern.get_mappings (),
					    pattern.get_locus (), ctx);
  tree lower = compile_range_pattern_bound (pattern.get_lower_bound ().get (),
					    pattern.get_mappings (),
					    pattern.get_locus (), ctx);

  tree check_lower
    = Backend::comparison_expression (ComparisonOperator::GREATER_OR_EQUAL,
				      match_scrutinee_expr, lower,
				      pattern.get_locus ());
  tree check_upper
    = Backend::comparison_expression (ComparisonOperator::LESS_OR_EQUAL,
				      match_scrutinee_expr, upper,
				      pattern.get_locus ());
  check_expr = Backend::arithmetic_or_logical_expression (
    ArithmeticOrLogicalOperator::BITWISE_AND, check_lower, check_upper,
    pattern.get_locus ());
}

void
CompilePatternCheckExpr::visit (HIR::ReferencePattern &pattern)
{
  match_scrutinee_expr
    = indirect_expression (match_scrutinee_expr, pattern.get_locus ());
  pattern.get_referenced_pattern ()->accept_vis (*this);
}

void
CompilePatternCheckExpr::visit (HIR::AltPattern &pattern)
{
  auto &alts = pattern.get_alts ();

  check_expr = CompilePatternCheckExpr::Compile (alts.at (0).get (),
						 match_scrutinee_expr, ctx);
  auto end = alts.end ();
  for (auto i = alts.begin () + 1; i != end; i++)
    {
      tree next_expr
	= CompilePatternCheckExpr::Compile (i->get (), match_scrutinee_expr,
					    ctx);
      check_expr = Backend::arithmetic_or_logical_expression (
	ArithmeticOrLogicalOperator::BITWISE_OR, check_expr, next_expr,
	(*i)->get_locus ());
    }
}

void
CompilePatternCheckExpr::visit (HIR::StructPattern &pattern)
{
  // lookup the type
  TyTy::BaseType *lookup = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    pattern.get_path ().get_mappings ().get_hirid (), &lookup);
  rust_assert (ok);

  // this might be an enum
  rust_assert (lookup->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (lookup);

  rust_assert (adt->number_of_variants () > 0);
  TyTy::VariantDef *variant = nullptr;
  if (adt->is_enum ())
    {
      // lookup the variant
      HirId variant_id;
      ok = ctx->get_tyctx ()->lookup_variant_definition (
	pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
      rust_assert (ok);

      int variant_index = 0;
      ok = adt->lookup_variant_by_id (variant_id, &variant, &variant_index);
      rust_assert (ok);

      // find expected discriminant
      // // need to access qualifier the field, if we use QUAL_UNION_TYPE this
      // // would be DECL_QUALIFIER i think. For now this will just access the
      // // first record field and its respective qualifier because it will
      // // always be set because this is all a big special union
      HIR::Expr *discrim_expr = variant->get_discriminant ();
      tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);

      // find discriminant field of scrutinee
      tree scrutinee_record_expr
	= Backend::struct_field_expression (match_scrutinee_expr, variant_index,
					    pattern.get_path ().get_locus ());
      tree scrutinee_expr_qualifier_expr
	= Backend::struct_field_expression (scrutinee_record_expr, 0,
					    pattern.get_path ().get_locus ());

      check_expr
	= Backend::comparison_expression (ComparisonOperator::EQUAL,
					  scrutinee_expr_qualifier_expr,
					  discrim_expr_node,
					  pattern.get_path ().get_locus ());

      match_scrutinee_expr = scrutinee_record_expr;
    }
  else
    {
      variant = adt->get_variants ().at (0);
      check_expr = boolean_true_node;
    }

  auto &struct_pattern_elems = pattern.get_struct_pattern_elems ();
  for (auto &field : struct_pattern_elems.get_struct_pattern_fields ())
    {
      switch (field->get_item_type ())
	{
	  case HIR::StructPatternField::ItemType::TUPLE_PAT: {
	    // TODO
	    rust_unreachable ();
	  }
	  break;

	  case HIR::StructPatternField::ItemType::IDENT_PAT: {
	    HIR::StructPatternFieldIdentPat &ident
	      = static_cast<HIR::StructPatternFieldIdentPat &> (*field.get ());

	    size_t offs = 0;
	    ok = variant->lookup_field (ident.get_identifier ().as_string (),
					nullptr, &offs);
	    rust_assert (ok);

	    // we may be offsetting by + 1 here since the first field in the
	    // record is always the discriminator
	    offs += adt->is_enum ();
	    tree field_expr
	      = Backend::struct_field_expression (match_scrutinee_expr, offs,
						  ident.get_locus ());

	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (ident.get_pattern ().get (),
						  field_expr, ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, ident.get_pattern ()->get_locus ());
	  }
	  break;

	  case HIR::StructPatternField::ItemType::IDENT: {
	    // ident pattern always matches - do nothing
	  }
	  break;
	}
    }
}

void
CompilePatternCheckExpr::visit (HIR::TupleStructPattern &pattern)
{
  size_t tuple_field_index;

  // lookup the type
  TyTy::BaseType *lookup = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    pattern.get_path ().get_mappings ().get_hirid (), &lookup);
  rust_assert (ok);

  // this might be an enum
  rust_assert (lookup->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (lookup);

  rust_assert (adt->number_of_variants () > 0);
  TyTy::VariantDef *variant = nullptr;
  if (adt->is_enum ())
    {
      // lookup the variant
      HirId variant_id;
      ok = ctx->get_tyctx ()->lookup_variant_definition (
	pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
      rust_assert (ok);

      int variant_index = 0;
      ok = adt->lookup_variant_by_id (variant_id, &variant, &variant_index);
      rust_assert (ok);

      // find expected discriminant
      HIR::Expr *discrim_expr = variant->get_discriminant ();
      tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);

      // find discriminant field of scrutinee
      tree scrutinee_record_expr
	= Backend::struct_field_expression (match_scrutinee_expr, variant_index,
					    pattern.get_path ().get_locus ());
      tree scrutinee_expr_qualifier_expr
	= Backend::struct_field_expression (scrutinee_record_expr, 0,
					    pattern.get_path ().get_locus ());

      check_expr
	= Backend::comparison_expression (ComparisonOperator::EQUAL,
					  scrutinee_expr_qualifier_expr,
					  discrim_expr_node,
					  pattern.get_path ().get_locus ());

      match_scrutinee_expr = scrutinee_record_expr;
      // we are offsetting by + 1 here since the first field in the record
      // is always the discriminator
      tuple_field_index = 1;
    }
  else
    {
      variant = adt->get_variants ().at (0);
      check_expr = boolean_true_node;
      tuple_field_index = 0;
    }

  std::unique_ptr<HIR::TupleStructItems> &items = pattern.get_items ();
  switch (items->get_item_type ())
    {
      case HIR::TupleStructItems::RANGED: {
	// TODO
	rust_unreachable ();
      }
      break;

      case HIR::TupleStructItems::MULTIPLE: {
	HIR::TupleStructItemsNoRange &items_no_range
	  = static_cast<HIR::TupleStructItemsNoRange &> (*items.get ());

	rust_assert (items_no_range.get_patterns ().size ()
		     == variant->num_fields ());

	for (auto &pattern : items_no_range.get_patterns ())
	  {
	    tree field_expr
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_field_index++,
						  pattern->get_locus ());

	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (pattern.get (), field_expr,
						  ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, pattern->get_locus ());
	  }
      }
      break;
    }
}

void
CompilePatternCheckExpr::visit (HIR::TuplePattern &pattern)
{
  check_expr = boolean_true_node;

  switch (pattern.get_items ()->get_item_type ())
    {
      case HIR::TuplePatternItems::RANGED: {
	// TODO
	gcc_unreachable ();
      }
      break;

      case HIR::TuplePatternItems::MULTIPLE: {
	auto &items = static_cast<HIR::TuplePatternItemsMultiple &> (
	  *pattern.get_items ());
	size_t tuple_field_index = 0;

	for (auto &pat : items.get_patterns ())
	  {
	    tree field_expr
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_field_index++,
						  pat->get_locus ());

	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (pat.get (), field_expr, ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, pat->get_locus ());
	  }
      }
    }
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
      case HIR::TupleStructItems::RANGED: {
	// TODO
	rust_unreachable ();
      }
      break;

      case HIR::TupleStructItems::MULTIPLE: {
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
		  = Backend::struct_field_expression (match_scrutinee_expr,
						      variant_index,
						      pattern->get_locus ());

		tree binding
		  = Backend::struct_field_expression (variant_accessor,
						      tuple_field_index++,
						      pattern->get_locus ());

		ctx->insert_pattern_binding (
		  pattern->get_mappings ().get_hirid (), binding);
	      }
	  }
	else
	  {
	    size_t tuple_field_index = 0;
	    for (auto &pattern : items_no_range.get_patterns ())
	      {
		tree variant_accessor = match_scrutinee_expr;

		tree binding
		  = Backend::struct_field_expression (variant_accessor,
						      tuple_field_index++,
						      pattern->get_locus ());

		ctx->insert_pattern_binding (
		  pattern->get_mappings ().get_hirid (), binding);
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
	    rust_unreachable ();
	  }
	  break;

	  case HIR::StructPatternField::ItemType::IDENT_PAT: {
	    // TODO
	    rust_unreachable ();
	  }
	  break;

	  case HIR::StructPatternField::ItemType::IDENT: {
	    HIR::StructPatternFieldIdent &ident
	      = static_cast<HIR::StructPatternFieldIdent &> (*field.get ());

	    size_t offs = 0;
	    ok = variant->lookup_field (ident.get_identifier ().as_string (),
					nullptr, &offs);
	    rust_assert (ok);

	    tree binding = error_mark_node;
	    if (adt->is_enum ())
	      {
		tree variant_accessor
		  = Backend::struct_field_expression (match_scrutinee_expr,
						      variant_index,
						      ident.get_locus ());

		// we are offsetting by + 1 here since the first field in the
		// record is always the discriminator
		binding = Backend::struct_field_expression (variant_accessor,
							    offs + 1,
							    ident.get_locus ());
	      }
	    else
	      {
		tree variant_accessor = match_scrutinee_expr;
		binding
		  = Backend::struct_field_expression (variant_accessor, offs,
						      ident.get_locus ());
	      }

	    ctx->insert_pattern_binding (ident.get_mappings ().get_hirid (),
					 binding);
	  }
	  break;
	}
    }
}

void
CompilePatternBindings::visit (HIR::ReferencePattern &pattern)
{
  tree derefed
    = indirect_expression (match_scrutinee_expr, pattern.get_locus ());

  CompilePatternBindings::Compile (pattern.get_referenced_pattern ().get (),
				   derefed, ctx);
}

void
CompilePatternBindings::visit (HIR::IdentifierPattern &pattern)
{
  ctx->insert_pattern_binding (pattern.get_mappings ().get_hirid (),
			       match_scrutinee_expr);
}

void
CompilePatternLet::visit (HIR::IdentifierPattern &pattern)
{
  Bvariable *var = nullptr;
  rust_assert (
    ctx->lookup_var_decl (pattern.get_mappings ().get_hirid (), &var));

  auto fnctx = ctx->peek_fn ();
  if (ty->is_unit ())
    {
      ctx->add_statement (init_expr);

      auto unit_type_init_expr = unit_expression (ctx, rval_locus);
      auto s = Backend::init_statement (fnctx.fndecl, var, unit_type_init_expr);
      ctx->add_statement (s);
    }
  else
    {
      auto s = Backend::init_statement (fnctx.fndecl, var, init_expr);
      ctx->add_statement (s);
    }
}

void
CompilePatternLet::visit (HIR::WildcardPattern &pattern)
{
  tree init_stmt = NULL;
  tree stmt_type = TyTyResolveCompile::compile (ctx, ty);

  Backend::temporary_variable (ctx->peek_fn ().fndecl, NULL_TREE, stmt_type,
			       init_expr, false, pattern.get_locus (),
			       &init_stmt);

  ctx->add_statement (init_stmt);
}

void
CompilePatternLet::visit (HIR::TuplePattern &pattern)
{
  rust_assert (pattern.has_tuple_pattern_items ());

  tree tuple_type = TyTyResolveCompile::compile (ctx, ty);
  tree init_stmt;
  Bvariable *tmp_var
    = Backend::temporary_variable (ctx->peek_fn ().fndecl, NULL_TREE,
				   tuple_type, init_expr, false,
				   pattern.get_locus (), &init_stmt);
  tree access_expr = Backend::var_expression (tmp_var, pattern.get_locus ());
  ctx->add_statement (init_stmt);

  switch (pattern.get_items ()->get_item_type ())
    {
      case HIR::TuplePatternItems::ItemType::RANGED: {
	size_t tuple_idx = 0;
	auto &items
	  = static_cast<HIR::TuplePatternItemsRanged &> (*pattern.get_items ());

	auto &items_lower = items.get_lower_patterns ();
	auto &items_upper = items.get_upper_patterns ();

	for (auto &sub : items_lower)
	  {
	    TyTy::BaseType *ty_sub = nullptr;
	    HirId sub_id = sub->get_mappings ().get_hirid ();
	    bool ok = ctx->get_tyctx ()->lookup_type (sub_id, &ty_sub);
	    rust_assert (ok);

	    tree sub_init
	      = Backend::struct_field_expression (access_expr, tuple_idx,
						  sub->get_locus ());
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
	    HirId sub_id = sub->get_mappings ().get_hirid ();
	    bool ok = ctx->get_tyctx ()->lookup_type (sub_id, &ty_sub);
	    rust_assert (ok);

	    tree sub_init
	      = Backend::struct_field_expression (access_expr, tuple_idx,
						  sub->get_locus ());
	    CompilePatternLet::Compile (sub.get (), sub_init, ty_sub,
					rval_locus, ctx);
	    tuple_idx++;
	  }

	return;
      }
      case HIR::TuplePatternItems::ItemType::MULTIPLE: {
	size_t tuple_idx = 0;
	auto &items = static_cast<HIR::TuplePatternItemsMultiple &> (
	  *pattern.get_items ());

	for (auto &sub : items.get_patterns ())
	  {
	    TyTy::BaseType *ty_sub = nullptr;
	    HirId sub_id = sub->get_mappings ().get_hirid ();
	    bool ok = ctx->get_tyctx ()->lookup_type (sub_id, &ty_sub);
	    rust_assert (ok);

	    tree sub_init
	      = Backend::struct_field_expression (access_expr, tuple_idx,
						  sub->get_locus ());
	    CompilePatternLet::Compile (sub.get (), sub_init, ty_sub,
					rval_locus, ctx);
	    tuple_idx++;
	  }

	return;
      }
      default: {
	rust_unreachable ();
      }
    }
}

} // namespace Compile
} // namespace Rust
