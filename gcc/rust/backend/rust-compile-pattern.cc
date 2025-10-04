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

#include "rust-compile-pattern.h"
#include "rust-compile-expr.h"
#include "rust-compile-resolve-path.h"
#include "rust-constexpr.h"
#include "rust-compile-type.h"
#include "print-tree.h"
#include "rust-diagnostics.h"
#include "rust-hir-pattern-abstract.h"
#include "rust-hir-pattern.h"
#include "rust-system.h"
#include "rust-tyty.h"
#include "tree.h"

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

  // must be an ADT (?)
  rust_assert (lookup->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (lookup);

  // if this isn't an enum, always succeed
  if (!adt->is_enum ())
    {
      check_expr = boolean_true_node;
      return;
    }

  // lookup the variant
  HirId variant_id;
  ok = ctx->get_tyctx ()->lookup_variant_definition (
    pattern.get_mappings ().get_hirid (), &variant_id);
  rust_assert (ok);

  TyTy::VariantDef *variant = nullptr;
  ok = adt->lookup_variant_by_id (variant_id, &variant);
  rust_assert (ok);

  // find discriminant field of scrutinee
  tree scrutinee_expr_qualifier_expr
    = Backend::struct_field_expression (match_scrutinee_expr, 0,
					pattern.get_locus ());

  // must be enum
  match_scrutinee_expr = scrutinee_expr_qualifier_expr;

  HIR::Expr &discrim_expr = variant->get_discriminant ();
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
  auto litexpr = std::make_unique<HIR::LiteralExpr> (
    HIR::LiteralExpr (pattern.get_mappings (), pattern.get_literal (),
		      pattern.get_locus (), std::vector<AST::Attribute> ()));

  // Note: Floating point literals are currently accepted but will likely be
  // forbidden in LiteralPatterns in a future version of Rust.
  // See: https://github.com/rust-lang/rust/issues/41620
  // For now, we cannot compile them anyway as CASE_LABEL_EXPR does not support
  // floating point types.
  if (pattern.get_literal ().get_lit_type () == HIR::Literal::LitType::FLOAT)
    {
      rust_sorry_at (pattern.get_locus (), "floating-point literal in pattern");
    }

  tree lit = CompileExpr::Compile (*litexpr, ctx);

  check_expr = Backend::comparison_expression (ComparisonOperator::EQUAL,
					       match_scrutinee_expr, lit,
					       pattern.get_locus ());
}

static tree
compile_range_pattern_bound (HIR::RangePatternBound &bound,
			     Analysis::NodeMapping mappings, location_t locus,
			     Context *ctx)
{
  tree result = NULL_TREE;
  switch (bound.get_bound_type ())
    {
    case HIR::RangePatternBound::RangePatternBoundType::LITERAL:
      {
	auto &ref = static_cast<HIR::RangePatternBoundLiteral &> (bound);

	HIR::LiteralExpr litexpr (mappings, ref.get_literal (), locus,
				  std::vector<AST::Attribute> ());

	result = CompileExpr::Compile (litexpr, ctx);
      }
      break;

    case HIR::RangePatternBound::RangePatternBoundType::PATH:
      {
	auto &ref = static_cast<HIR::RangePatternBoundPath &> (bound);

	result = ResolvePathRef::Compile (ref.get_path (), ctx);

	// If the path resolves to a const expression, fold it.
	result = fold_expr (result);
      }
      break;

    case HIR::RangePatternBound::RangePatternBoundType::QUALPATH:
      {
	auto &ref = static_cast<HIR::RangePatternBoundQualPath &> (bound);

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
  tree upper = compile_range_pattern_bound (pattern.get_upper_bound (),
					    pattern.get_mappings (),
					    pattern.get_locus (), ctx);
  tree lower = compile_range_pattern_bound (pattern.get_lower_bound (),
					    pattern.get_mappings (),
					    pattern.get_locus (), ctx);

  ComparisonOperator upper_cmp = pattern.is_inclusive_range ()
				   ? ComparisonOperator::LESS_OR_EQUAL
				   : ComparisonOperator::LESS_THAN;
  tree check_lower
    = Backend::comparison_expression (ComparisonOperator::GREATER_OR_EQUAL,
				      match_scrutinee_expr, lower,
				      pattern.get_locus ());
  tree check_upper
    = Backend::comparison_expression (upper_cmp, match_scrutinee_expr, upper,
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
  pattern.get_referenced_pattern ().accept_vis (*this);
}

void
CompilePatternCheckExpr::visit (HIR::AltPattern &pattern)
{
  auto &alts = pattern.get_alts ();

  check_expr = CompilePatternCheckExpr::Compile (*alts.at (0),
						 match_scrutinee_expr, ctx);
  auto end = alts.end ();
  for (auto i = alts.begin () + 1; i != end; i++)
    {
      tree next_expr
	= CompilePatternCheckExpr::Compile (**i, match_scrutinee_expr, ctx);
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
  tree variant_accesser_expr = nullptr;
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
      // // would be DECL_QUALIFIER i think.
      HIR::Expr &discrim_expr = variant->get_discriminant ();
      tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);

      // find discriminant field of scrutinee
      tree scrutinee_expr_qualifier_expr
	= Backend::struct_field_expression (match_scrutinee_expr, 0,
					    pattern.get_path ().get_locus ());

      // access variant data
      tree scrutinee_union_expr
	= Backend::struct_field_expression (match_scrutinee_expr, 1,
					    pattern.get_path ().get_locus ());
      variant_accesser_expr
	= Backend::struct_field_expression (scrutinee_union_expr, variant_index,
					    pattern.get_path ().get_locus ());

      check_expr
	= Backend::comparison_expression (ComparisonOperator::EQUAL,
					  scrutinee_expr_qualifier_expr,
					  discrim_expr_node,
					  pattern.get_path ().get_locus ());

      match_scrutinee_expr = scrutinee_expr_qualifier_expr;
    }
  else
    {
      variant = adt->get_variants ().at (0);
      variant_accesser_expr = match_scrutinee_expr;
      check_expr = boolean_true_node;
    }

  auto &struct_pattern_elems = pattern.get_struct_pattern_elems ();
  for (auto &field : struct_pattern_elems.get_struct_pattern_fields ())
    {
      switch (field->get_item_type ())
	{
	case HIR::StructPatternField::ItemType::TUPLE_PAT:
	  {
	    // TODO
	    rust_unreachable ();
	  }
	  break;

	case HIR::StructPatternField::ItemType::IDENT_PAT:
	  {
	    HIR::StructPatternFieldIdentPat &ident
	      = static_cast<HIR::StructPatternFieldIdentPat &> (*field.get ());

	    size_t offs = 0;
	    ok = variant->lookup_field (ident.get_identifier ().as_string (),
					nullptr, &offs);
	    rust_assert (ok);

	    tree field_expr
	      = Backend::struct_field_expression (variant_accesser_expr, offs,
						  ident.get_locus ());

	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (ident.get_pattern (),
						  field_expr, ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, ident.get_pattern ().get_locus ());
	  }
	  break;

	case HIR::StructPatternField::ItemType::IDENT:
	  {
	    // ident pattern always matches - do nothing
	  }
	  break;
	}
    }
}

void
CompilePatternCheckExpr::visit (HIR::TupleStructPattern &pattern)
{
  // lookup the type
  TyTy::BaseType *lookup = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    pattern.get_path ().get_mappings ().get_hirid (), &lookup);
  rust_assert (ok);

  // this might be an enum
  rust_assert (lookup->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (lookup);

  int variant_index = 0;
  rust_assert (adt->number_of_variants () > 0);
  TyTy::VariantDef *variant = nullptr;
  if (adt->is_enum ())
    {
      // lookup the variant
      HirId variant_id;
      ok = ctx->get_tyctx ()->lookup_variant_definition (
	pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
      rust_assert (ok);

      ok = adt->lookup_variant_by_id (variant_id, &variant, &variant_index);
      rust_assert (ok);

      // find expected discriminant
      HIR::Expr &discrim_expr = variant->get_discriminant ();
      tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);

      // find discriminant field of scrutinee
      tree scrutinee_expr_qualifier_expr
	= Backend::struct_field_expression (match_scrutinee_expr, 0,
					    pattern.get_path ().get_locus ());

      check_expr
	= Backend::comparison_expression (ComparisonOperator::EQUAL,
					  scrutinee_expr_qualifier_expr,
					  discrim_expr_node,
					  pattern.get_path ().get_locus ());
    }
  else
    {
      variant = adt->get_variants ().at (0);
      check_expr = boolean_true_node;
    }

  HIR::TupleStructItems &items = pattern.get_items ();
  switch (items.get_item_type ())
    {
    case HIR::TupleStructItems::HAS_REST:
      {
	HIR::TupleStructItemsHasRest &items_has_rest
	  = static_cast<HIR::TupleStructItemsHasRest &> (items);
	size_t num_patterns = items_has_rest.get_lower_patterns ().size ()
			      + items_has_rest.get_upper_patterns ().size ();

	// enums cases shouldn't reach here
	rust_assert (num_patterns <= variant->num_fields ()
		     && (!adt->is_enum ()));

	size_t tuple_field_index = 0;
	for (auto &pattern : items_has_rest.get_lower_patterns ())
	  {
	    tree field_expr
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_field_index++,
						  pattern->get_locus ());
	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (*pattern, field_expr, ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, pattern->get_locus ());
	  }
	tuple_field_index = variant->num_fields ()
			    - items_has_rest.get_upper_patterns ().size ();
	for (auto &pattern : items_has_rest.get_upper_patterns ())
	  {
	    tree field_expr
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_field_index++,
						  pattern->get_locus ());
	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (*pattern, field_expr, ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, pattern->get_locus ());
	  }
      }
      break;

    case HIR::TupleStructItems::NO_REST:
      {
	HIR::TupleStructItemsNoRest &items_no_range
	  = static_cast<HIR::TupleStructItemsNoRest &> (items);

	rust_assert (items_no_range.get_patterns ().size ()
		     == variant->num_fields ());

	if (adt->is_enum ())
	  {
	    size_t tuple_field_index = 0;
	    for (auto &pattern : items_no_range.get_patterns ())
	      {
		// find payload union field of scrutinee
		tree payload_ref
		  = Backend::struct_field_expression (match_scrutinee_expr, 1,
						      pattern->get_locus ());

		tree variant_ref
		  = Backend::struct_field_expression (payload_ref,
						      variant_index,
						      pattern->get_locus ());

		tree field_expr
		  = Backend::struct_field_expression (variant_ref,
						      tuple_field_index++,
						      pattern->get_locus ());

		tree check_expr_sub
		  = CompilePatternCheckExpr::Compile (*pattern, field_expr,
						      ctx);
		check_expr = Backend::arithmetic_or_logical_expression (
		  ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
		  check_expr_sub, pattern->get_locus ());
	      }
	  }
	else
	  {
	    // For non-enum TupleStructPatterns
	    size_t tuple_field_index = 0;
	    for (auto &pattern : items_no_range.get_patterns ())
	      {
		tree field_expr
		  = Backend::struct_field_expression (match_scrutinee_expr,
						      tuple_field_index++,
						      pattern->get_locus ());

		tree check_expr_sub
		  = CompilePatternCheckExpr::Compile (*pattern, field_expr,
						      ctx);
		check_expr = Backend::arithmetic_or_logical_expression (
		  ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
		  check_expr_sub, pattern->get_locus ());
	      }
	  }
	break;
      }
    }
}

void
CompilePatternCheckExpr::visit (HIR::TuplePattern &pattern)
{
  check_expr = boolean_true_node;

  switch (pattern.get_items ().get_item_type ())
    {
    case HIR::TuplePatternItems::HAS_REST:
      {
	auto &items
	  = static_cast<HIR::TuplePatternItemsHasRest &> (pattern.get_items ());
	size_t tuple_field_index = 0;

	// lookup the type to find out number of fields
	TyTy::BaseType *ty = nullptr;
	bool ok = ctx->get_tyctx ()->lookup_type (
	  pattern.get_mappings ().get_hirid (), &ty);
	rust_assert (ok);
	rust_assert (ty->get_kind () == TyTy::TypeKind::TUPLE);

	// compile check expr for lower patterns
	for (auto &pat : items.get_lower_patterns ())
	  {
	    tree field_expr
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_field_index++,
						  pat->get_locus ());

	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (*pat, field_expr, ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, pat->get_locus ());
	  }

	// skip the fields that are not checked
	tuple_field_index = static_cast<TyTy::TupleType &> (*ty).num_fields ()
			    - items.get_upper_patterns ().size ();

	// compile check expr for upper patterns
	for (auto &pat : items.get_upper_patterns ())
	  {
	    tree field_expr
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_field_index++,
						  pat->get_locus ());

	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (*pat, field_expr, ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, pat->get_locus ());
	  }
      }
      break;

    case HIR::TuplePatternItems::NO_REST:
      {
	auto &items
	  = static_cast<HIR::TuplePatternItemsNoRest &> (pattern.get_items ());
	size_t tuple_field_index = 0;

	for (auto &pat : items.get_patterns ())
	  {
	    tree field_expr
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_field_index++,
						  pat->get_locus ());

	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (*pat, field_expr, ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, pat->get_locus ());
	  }
      }
    }
}

void
CompilePatternCheckExpr::visit (HIR::IdentifierPattern &pattern)
{
  if (pattern.has_subpattern ())
    {
      check_expr = CompilePatternCheckExpr::Compile (pattern.get_subpattern (),
						     match_scrutinee_expr, ctx);
    }
  else
    {
      check_expr = boolean_true_node;
    }
}

void
CompilePatternCheckExpr::visit (HIR::SlicePattern &pattern)
{
  check_expr = boolean_true_node;

  // lookup the type
  TyTy::BaseType *lookup = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (pattern.get_mappings ().get_hirid (),
				      &lookup);
  rust_assert (ok);

  // pattern must either be ArrayType or SliceType, should be already confirmed
  // by type checking
  rust_assert (lookup->get_kind () == TyTy::TypeKind::ARRAY
	       || lookup->get_kind () == TyTy::TypeKind::SLICE
	       || lookup->get_kind () == TyTy::REF);

  // function ptr that points to either array_index_expression or
  // slice_index_expression depending on the scrutinee's type
  tree (*scrutinee_index_expr_func) (tree, tree, location_t) = nullptr;

  switch (lookup->get_kind ())
    {
    case TyTy::TypeKind::ARRAY:
      scrutinee_index_expr_func = Backend::array_index_expression;
      break;
    case TyTy::TypeKind::SLICE:
      rust_sorry_at (
	pattern.get_locus (),
	"SlicePattern matching against non-ref slices are not yet supported");
      break;
    case TyTy::TypeKind::REF:
      {
	rust_assert (RS_DST_FLAG_P (TREE_TYPE (match_scrutinee_expr)));
	scrutinee_index_expr_func = Backend::slice_index_expression;
	tree size_field
	  = Backend::struct_field_expression (match_scrutinee_expr, 1,
					      pattern.get_locus ());

	// for slices, generate a dynamic size comparison expression tree
	// because size checking is done at runtime.
	switch (pattern.get_items ().get_item_type ())
	  {
	  case HIR::SlicePatternItems::ItemType::NO_REST:
	    {
	      auto &items = static_cast<HIR::SlicePatternItemsNoRest &> (
		pattern.get_items ());
	      check_expr = Backend::comparison_expression (
		ComparisonOperator::EQUAL, size_field,
		build_int_cst (size_type_node, items.get_patterns ().size ()),
		pattern.get_locus ());
	    }
	    break;
	  case HIR::SlicePatternItems::ItemType::HAS_REST:
	    {
	      auto &items = static_cast<HIR::SlicePatternItemsHasRest &> (
		pattern.get_items ());
	      auto pattern_min_cap = items.get_lower_patterns ().size ()
				     + items.get_upper_patterns ().size ();
	      check_expr = Backend::comparison_expression (
		ComparisonOperator::GREATER_OR_EQUAL, size_field,
		build_int_cst (size_type_node, pattern_min_cap),
		pattern.get_locus ());
	    }
	    break;
	  }
      }
      break;
    default:
      rust_unreachable ();
    }

  rust_assert (scrutinee_index_expr_func != nullptr);

  // Generate tree to compare every element within array/slice
  size_t element_index = 0;
  switch (pattern.get_items ().get_item_type ())
    {
    case HIR::SlicePatternItems::ItemType::NO_REST:
      {
	auto &items
	  = static_cast<HIR::SlicePatternItemsNoRest &> (pattern.get_items ());
	for (auto &pattern_member : items.get_patterns ())
	  {
	    tree index_tree
	      = Backend::size_constant_expression (element_index++);
	    tree element_expr
	      = scrutinee_index_expr_func (match_scrutinee_expr, index_tree,
					   pattern.get_locus ());
	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (*pattern_member, element_expr,
						  ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, pattern.get_locus ());
	  }
	break;
      }
    case HIR::SlicePatternItems::ItemType::HAS_REST:
      {
	auto &items
	  = static_cast<HIR::SlicePatternItemsHasRest &> (pattern.get_items ());
	for (auto &pattern_member : items.get_lower_patterns ())
	  {
	    tree index_tree
	      = Backend::size_constant_expression (element_index++);
	    tree element_expr
	      = scrutinee_index_expr_func (match_scrutinee_expr, index_tree,
					   pattern.get_locus ());
	    tree check_expr_sub
	      = CompilePatternCheckExpr::Compile (*pattern_member, element_expr,
						  ctx);
	    check_expr = Backend::arithmetic_or_logical_expression (
	      ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
	      check_expr_sub, pattern.get_locus ());
	  }

	// handle codegen for upper patterns differently for both types
	switch (lookup->get_kind ())
	  {
	  case TyTy::TypeKind::ARRAY:
	    {
	      // for array type scrutinee, we can simply get the capacity as a
	      // const and calculate how many elements to skip
	      auto array_ty = static_cast<TyTy::ArrayType *> (lookup);
	      auto capacity_ty = array_ty->get_capacity ();

	      rust_assert (capacity_ty->get_kind () == TyTy::TypeKind::CONST);
	      auto *capacity_const = capacity_ty->as_const_type ();
	      rust_assert (capacity_const->const_kind ()
			   == TyTy::BaseConstType::ConstKind::Value);
	      auto &capacity_value
		= *static_cast<TyTy::ConstValueType *> (capacity_const);
	      auto cap_tree = capacity_value.get_value ();

	      rust_assert (!error_operand_p (cap_tree));

	      size_t cap_wi = (size_t) wi::to_wide (cap_tree).to_uhwi ();
	      element_index = cap_wi - items.get_upper_patterns ().size ();
	      for (auto &pattern_member : items.get_upper_patterns ())
		{
		  tree index_tree
		    = Backend::size_constant_expression (element_index++);
		  tree element_expr
		    = scrutinee_index_expr_func (match_scrutinee_expr,
						 index_tree,
						 pattern.get_locus ());
		  tree check_expr_sub
		    = CompilePatternCheckExpr::Compile (*pattern_member,
							element_expr, ctx);
		  check_expr = Backend::arithmetic_or_logical_expression (
		    ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
		    check_expr_sub, pattern.get_locus ());
		}
	    }
	    break;
	  case TyTy::TypeKind::REF:
	    {
	      // for slice type scrutinee, size is dyanamic, so number of
	      // elements to skip is calculated during runtime
	      tree slice_size
		= Backend::struct_field_expression (match_scrutinee_expr, 1,
						    pattern.get_locus ());
	      tree upper_patterns_size = Backend::size_constant_expression (
		items.get_upper_patterns ().size ());
	      tree index_tree = Backend::arithmetic_or_logical_expression (
		ArithmeticOrLogicalOperator::SUBTRACT, slice_size,
		upper_patterns_size, pattern.get_locus ());
	      for (auto &pattern_member : items.get_upper_patterns ())
		{
		  tree element_expr
		    = scrutinee_index_expr_func (match_scrutinee_expr,
						 index_tree,
						 pattern.get_locus ());
		  tree check_expr_sub
		    = CompilePatternCheckExpr::Compile (*pattern_member,
							element_expr, ctx);
		  check_expr = Backend::arithmetic_or_logical_expression (
		    ArithmeticOrLogicalOperator::BITWISE_AND, check_expr,
		    check_expr_sub, pattern.get_locus ());
		  index_tree = Backend::arithmetic_or_logical_expression (
		    ArithmeticOrLogicalOperator::ADD, index_tree,
		    Backend::size_constant_expression (1),
		    pattern.get_locus ());
		}
	    }
	    break;
	  default:
	    rust_unreachable ();
	  }
      }
      break;
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

  HIR::TupleStructItems &items = pattern.get_items ();
  switch (items.get_item_type ())
    {
    case HIR::TupleStructItems::HAS_REST:
      {
	HIR::TupleStructItemsHasRest &items_has_rest
	  = static_cast<HIR::TupleStructItemsHasRest &> (items);
	size_t num_patterns = items_has_rest.get_lower_patterns ().size ()
			      + items_has_rest.get_upper_patterns ().size ();

	// enums cases shouldn't reach here
	rust_assert (num_patterns <= variant->num_fields ()
		     && (!adt->is_enum ()));

	size_t tuple_field_index = 0;
	for (auto &pattern : items_has_rest.get_lower_patterns ())
	  {
	    tree binding
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_field_index++,
						  pattern->get_locus ());

	    CompilePatternBindings::Compile (*pattern, binding, ctx);
	  }

	tuple_field_index = variant->num_fields ()
			    - items_has_rest.get_upper_patterns ().size ();

	for (auto &pattern : items_has_rest.get_upper_patterns ())
	  {
	    tree binding
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_field_index++,
						  pattern->get_locus ());

	    CompilePatternBindings::Compile (*pattern, binding, ctx);
	  }
      }
      break;

    case HIR::TupleStructItems::NO_REST:
      {
	HIR::TupleStructItemsNoRest &items_no_rest
	  = static_cast<HIR::TupleStructItemsNoRest &> (items);
	rust_assert (items_no_rest.get_patterns ().size ()
		     == variant->num_fields ());

	if (adt->is_enum ())
	  {
	    size_t tuple_field_index = 0;
	    for (auto &pattern : items_no_rest.get_patterns ())
	      {
		tree payload_accessor_union
		  = Backend::struct_field_expression (match_scrutinee_expr, 1,
						      pattern->get_locus ());

		tree variant_accessor
		  = Backend::struct_field_expression (payload_accessor_union,
						      variant_index,
						      pattern->get_locus ());

		tree binding
		  = Backend::struct_field_expression (variant_accessor,
						      tuple_field_index++,
						      pattern->get_locus ());

		CompilePatternBindings::Compile (*pattern, binding, ctx);
	      }
	  }
	else
	  {
	    size_t tuple_field_index = 0;
	    for (auto &pattern : items_no_rest.get_patterns ())
	      {
		tree binding
		  = Backend::struct_field_expression (match_scrutinee_expr,
						      tuple_field_index++,
						      pattern->get_locus ());

		CompilePatternBindings::Compile (*pattern, binding, ctx);
	      }
	  }
      }
      break;
    }
}

tree
CompilePatternBindings::make_struct_access (TyTy::ADTType *adt,
					    TyTy::VariantDef *variant,
					    const Identifier &ident,
					    int variant_index)
{
  size_t offs = 0;
  auto ok = variant->lookup_field (ident.as_string (), nullptr, &offs);
  rust_assert (ok);

  if (adt->is_enum ())
    {
      tree payload_accessor_union
	= Backend::struct_field_expression (match_scrutinee_expr, 1,
					    ident.get_locus ());

      tree variant_accessor
	= Backend::struct_field_expression (payload_accessor_union,
					    variant_index, ident.get_locus ());

      return Backend::struct_field_expression (variant_accessor, offs,
					       ident.get_locus ());
    }
  else
    {
      tree variant_accessor = match_scrutinee_expr;

      return Backend::struct_field_expression (variant_accessor, offs,
					       ident.get_locus ());
    }
}

void
CompilePatternBindings::handle_struct_pattern_ident (
  HIR::StructPatternField &pat, TyTy::ADTType *adt, TyTy::VariantDef *variant,
  int variant_index)
{
  HIR::StructPatternFieldIdent &ident
    = static_cast<HIR::StructPatternFieldIdent &> (pat);

  auto identifier = ident.get_identifier ();
  tree binding = make_struct_access (adt, variant, identifier, variant_index);

  ctx->insert_pattern_binding (ident.get_mappings ().get_hirid (), binding);
}

void
CompilePatternBindings::handle_struct_pattern_ident_pat (
  HIR::StructPatternField &pat, TyTy::ADTType *adt, TyTy::VariantDef *variant,
  int variant_index)
{
  auto &pattern = static_cast<HIR::StructPatternFieldIdentPat &> (pat);

  tree binding = make_struct_access (adt, variant, pattern.get_identifier (),
				     variant_index);
  CompilePatternBindings::Compile (pattern.get_pattern (), binding, ctx);
}

void
CompilePatternBindings::handle_struct_pattern_tuple_pat (
  HIR::StructPatternField &pat)
{
  rust_unreachable ();
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

  rust_assert (
    variant->get_variant_type () == TyTy::VariantDef::VariantType::STRUCT
    || variant->get_variant_type () == TyTy::VariantDef::VariantType::TUPLE);

  auto &struct_pattern_elems = pattern.get_struct_pattern_elems ();
  for (auto &field : struct_pattern_elems.get_struct_pattern_fields ())
    {
      switch (field->get_item_type ())
	{
	case HIR::StructPatternField::ItemType::TUPLE_PAT:
	  handle_struct_pattern_tuple_pat (*field);
	  break;
	case HIR::StructPatternField::ItemType::IDENT_PAT:
	  handle_struct_pattern_ident_pat (*field, adt, variant, variant_index);
	  break;
	case HIR::StructPatternField::ItemType::IDENT:
	  handle_struct_pattern_ident (*field, adt, variant, variant_index);
	  break;
	}
    }
}

void
CompilePatternBindings::visit (HIR::ReferencePattern &pattern)
{
  tree derefed
    = indirect_expression (match_scrutinee_expr, pattern.get_locus ());

  CompilePatternBindings::Compile (pattern.get_referenced_pattern (), derefed,
				   ctx);
}

void
CompilePatternBindings::visit (HIR::IdentifierPattern &pattern)
{
  if (pattern.has_subpattern ())
    {
      CompilePatternBindings::Compile (pattern.get_subpattern (),
				       match_scrutinee_expr, ctx);
    }

  if (!pattern.get_is_ref ())
    {
      ctx->insert_pattern_binding (pattern.get_mappings ().get_hirid (),
				   match_scrutinee_expr);
      return;
    }

  tree ref = address_expression (match_scrutinee_expr,
				 EXPR_LOCATION (match_scrutinee_expr));
  ctx->insert_pattern_binding (pattern.get_mappings ().get_hirid (), ref);
}

void
CompilePatternBindings::visit (HIR::TuplePattern &pattern)
{
  rust_assert (pattern.has_tuple_pattern_items ());

  // lookup the type
  TyTy::BaseType *ty = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (pattern.get_mappings ().get_hirid (),
				      &ty);
  rust_assert (ok);

  switch (pattern.get_items ().get_item_type ())
    {
    case HIR::TuplePatternItems::ItemType::HAS_REST:
      {
	size_t tuple_idx = 0;
	auto &items
	  = static_cast<HIR::TuplePatternItemsHasRest &> (pattern.get_items ());

	auto &items_lower = items.get_lower_patterns ();
	auto &items_upper = items.get_upper_patterns ();

	for (auto &sub : items_lower)
	  {
	    TyTy::BaseType *ty_sub = nullptr;
	    HirId sub_id = sub->get_mappings ().get_hirid ();
	    bool ok = ctx->get_tyctx ()->lookup_type (sub_id, &ty_sub);
	    rust_assert (ok);

	    tree sub_init
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_idx, sub->get_locus ());

	    CompilePatternBindings::Compile (*sub.get (), sub_init, ctx);
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
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_idx, sub->get_locus ());
	    CompilePatternBindings::Compile (*sub.get (), sub_init, ctx);
	    tuple_idx++;
	  }

	return;
      }
    case HIR::TuplePatternItems::ItemType::NO_REST:
      {
	size_t tuple_idx = 0;
	auto &items
	  = static_cast<HIR::TuplePatternItemsNoRest &> (pattern.get_items ());

	for (auto &sub : items.get_patterns ())
	  {
	    TyTy::BaseType *ty_sub = nullptr;
	    HirId sub_id = sub->get_mappings ().get_hirid ();
	    bool ok = ctx->get_tyctx ()->lookup_type (sub_id, &ty_sub);
	    rust_assert (ok);

	    tree sub_init
	      = Backend::struct_field_expression (match_scrutinee_expr,
						  tuple_idx, sub->get_locus ());
	    CompilePatternBindings::Compile (*sub.get (), sub_init, ctx);
	    tuple_idx++;
	  }

	return;
      }
    default:
      {
	rust_unreachable ();
      }
    }
}

void
CompilePatternBindings::visit (HIR::SlicePattern &pattern)
{
  // lookup the type
  TyTy::BaseType *lookup = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (pattern.get_mappings ().get_hirid (),
				      &lookup);
  rust_assert (ok);

  rust_assert (lookup->get_kind () == TyTy::TypeKind::ARRAY
	       || lookup->get_kind () == TyTy::TypeKind::SLICE
	       || lookup->get_kind () == TyTy::REF);

  // function ptr that points to either array_index_expression or
  // slice_index_expression depending on the scrutinee's type
  tree (*scrutinee_index_expr_func) (tree, tree, location_t) = nullptr;

  switch (lookup->get_kind ())
    {
    case TyTy::TypeKind::ARRAY:
      scrutinee_index_expr_func = Backend::array_index_expression;
      break;
    case TyTy::TypeKind::SLICE:
      rust_sorry_at (pattern.get_locus (),
		     "SlicePattern matching against non-ref slices are "
		     "not yet supported");
      break;
    case TyTy::TypeKind::REF:
      scrutinee_index_expr_func = Backend::slice_index_expression;
      break;
    default:
      rust_unreachable ();
    }

  rust_assert (scrutinee_index_expr_func != nullptr);

  size_t element_index = 0;

  switch (pattern.get_items ().get_item_type ())
    {
    case HIR::SlicePatternItems::ItemType::NO_REST:
      {
	auto &items
	  = static_cast<HIR::SlicePatternItemsNoRest &> (pattern.get_items ());
	for (auto &pattern_member : items.get_patterns ())
	  {
	    tree index_tree
	      = Backend::size_constant_expression (element_index++);
	    tree element_expr
	      = scrutinee_index_expr_func (match_scrutinee_expr, index_tree,
					   pattern.get_locus ());
	    CompilePatternBindings::Compile (*pattern_member, element_expr,
					     ctx);
	  }
      }
      break;
    case HIR::SlicePatternItems::ItemType::HAS_REST:
      {
	auto &items
	  = static_cast<HIR::SlicePatternItemsHasRest &> (pattern.get_items ());
	for (auto &pattern_member : items.get_lower_patterns ())
	  {
	    tree index_tree
	      = Backend::size_constant_expression (element_index++);
	    tree element_expr
	      = scrutinee_index_expr_func (match_scrutinee_expr, index_tree,
					   pattern.get_locus ());
	    CompilePatternBindings::Compile (*pattern_member, element_expr,
					     ctx);
	  }

	// handle codegen for upper patterns differently for both types
	switch (lookup->get_kind ())
	  {
	  case TyTy::TypeKind::ARRAY:
	    {
	      auto array_ty = static_cast<TyTy::ArrayType *> (lookup);
	      auto capacity_ty = array_ty->get_capacity ();

	      rust_assert (capacity_ty->get_kind () == TyTy::TypeKind::CONST);
	      auto *capacity_const = capacity_ty->as_const_type ();
	      rust_assert (capacity_const->const_kind ()
			   == TyTy::BaseConstType::ConstKind::Value);
	      auto &capacity_value
		= *static_cast<TyTy::ConstValueType *> (capacity_const);
	      auto cap_tree = capacity_value.get_value ();

	      rust_assert (!error_operand_p (cap_tree));

	      size_t cap_wi = (size_t) wi::to_wide (cap_tree).to_uhwi ();
	      element_index = cap_wi - items.get_upper_patterns ().size ();
	      for (auto &pattern_member : items.get_upper_patterns ())
		{
		  tree index_tree
		    = Backend::size_constant_expression (element_index++);
		  tree element_expr
		    = scrutinee_index_expr_func (match_scrutinee_expr,
						 index_tree,
						 pattern.get_locus ());
		  CompilePatternBindings::Compile (*pattern_member,
						   element_expr, ctx);
		}
	    }
	    break;
	  case TyTy::TypeKind::SLICE:
	    rust_sorry_at (pattern.get_locus (),
			   "SlicePattern matching against non-ref slices are "
			   "not yet supported");
	    break;
	  case TyTy::TypeKind::REF:
	    {
	      tree slice_size
		= Backend::struct_field_expression (match_scrutinee_expr, 1,
						    pattern.get_locus ());
	      tree upper_patterns_size = Backend::size_constant_expression (
		items.get_upper_patterns ().size ());
	      tree index_tree = Backend::arithmetic_or_logical_expression (
		ArithmeticOrLogicalOperator::SUBTRACT, slice_size,
		upper_patterns_size, pattern.get_locus ());
	      for (auto &pattern_member : items.get_upper_patterns ())
		{
		  tree element_expr
		    = scrutinee_index_expr_func (match_scrutinee_expr,
						 index_tree,
						 pattern.get_locus ());
		  CompilePatternBindings::Compile (*pattern_member,
						   element_expr, ctx);
		  index_tree = Backend::arithmetic_or_logical_expression (
		    ArithmeticOrLogicalOperator::ADD, index_tree,
		    Backend::size_constant_expression (1),
		    pattern.get_locus ());
		}
	    }
	    break;
	  default:
	    rust_unreachable ();
	  }
      }
      break;
    }
}

//

void
CompilePatternLet::visit (HIR::IdentifierPattern &pattern)
{
  Bvariable *var = nullptr;
  rust_assert (
    ctx->lookup_var_decl (pattern.get_mappings ().get_hirid (), &var));

  if (pattern.get_is_ref ())
    {
      init_expr = address_expression (init_expr, EXPR_LOCATION (init_expr));
    }

  auto fnctx = ctx->peek_fn ();
  if (ty->is_unit ())
    {
      ctx->add_statement (init_expr);

      auto unit_type_init_expr = unit_expression (rval_locus);
      auto s = Backend::init_statement (fnctx.fndecl, var, unit_type_init_expr);
      ctx->add_statement (s);
    }
  else
    {
      if (pattern.has_subpattern ())
	{
	  CompilePatternLet::Compile (&pattern.get_subpattern (), init_expr, ty,
				      rval_locus, ctx);
	}
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

  bool has_by_ref = false;
  auto check_refs
    = [] (const std::vector<std::unique_ptr<HIR::Pattern>> &patterns) {
	for (const auto &sub : patterns)
	  {
	    switch (sub->get_pattern_type ())
	      {
	      case HIR::Pattern::PatternType::IDENTIFIER:
		{
		  auto id = static_cast<HIR::IdentifierPattern *> (sub.get ());
		  if (id->get_is_ref ())
		    return true;
		  break;
		}
	      case HIR::Pattern::PatternType::REFERENCE:
		return true;
	      default:
		break;
	      }
	  }
	return false;
      };
  switch (pattern.get_items ().get_item_type ())
    {
    case HIR::TuplePatternItems::ItemType::NO_REST:
      {
	auto &items
	  = static_cast<HIR::TuplePatternItemsNoRest &> (pattern.get_items ());
	has_by_ref = check_refs (items.get_patterns ());
	break;
      }
    case HIR::TuplePatternItems::ItemType::HAS_REST:
      {
	auto &items
	  = static_cast<HIR::TuplePatternItemsHasRest &> (pattern.get_items ());
	has_by_ref = check_refs (items.get_lower_patterns ())
		     || check_refs (items.get_upper_patterns ());
	break;
      }
    default:
      break;
    }

  tree rhs_tuple_type = TYPE_MAIN_VARIANT (TREE_TYPE (init_expr));
  tree init_stmt;
  Bvariable *tmp_var
    = Backend::temporary_variable (ctx->peek_fn ().fndecl, NULL_TREE,
				   rhs_tuple_type, init_expr, has_by_ref,
				   pattern.get_locus (), &init_stmt);
  tree access_expr = Backend::var_expression (tmp_var, pattern.get_locus ());
  ctx->add_statement (init_stmt);

  switch (pattern.get_items ().get_item_type ())
    {
    case HIR::TuplePatternItems::ItemType::HAS_REST:
      {
	size_t tuple_idx = 0;
	auto &items
	  = static_cast<HIR::TuplePatternItemsHasRest &> (pattern.get_items ());

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
    case HIR::TuplePatternItems::ItemType::NO_REST:
      {
	size_t tuple_idx = 0;
	auto &items
	  = static_cast<HIR::TuplePatternItemsNoRest &> (pattern.get_items ());

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
    default:
      {
	rust_unreachable ();
      }
    }
}

} // namespace Compile
} // namespace Rust
