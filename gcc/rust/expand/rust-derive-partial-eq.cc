// Copyright (C) 2025 Free Software Foundation, Inc.

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

#include "rust-derive-partial-eq.h"
#include "rust-ast.h"
#include "rust-expr.h"
#include "rust-item.h"
#include "rust-operators.h"
#include "rust-path.h"
#include "rust-pattern.h"
#include "rust-system.h"

namespace Rust {
namespace AST {
DerivePartialEq::DerivePartialEq (location_t loc) : DeriveVisitor (loc) {}

std::vector<std::unique_ptr<AST::Item>>
DerivePartialEq::go (Item &item)
{
  item.accept_vis (*this);

  return std::move (expanded);
}

std::vector<std::unique_ptr<Item>>
DerivePartialEq::partialeq_impls (
  std::unique_ptr<AssociatedItem> &&eq_fn, std::string name,
  const std::vector<std::unique_ptr<GenericParam>> &type_generics)
{
  auto eq = builder.type_path (LangItem::Kind::EQ);
  auto speq = builder.type_path (LangItem::Kind::STRUCTURAL_PEQ);

  auto trait_items = vec (std::move (eq_fn));

  // no extra bound on StructuralPeq
  auto peq_generics
    = setup_impl_generics (name, type_generics, builder.trait_bound (eq));
  auto speq_generics = setup_impl_generics (name, type_generics);

  auto peq = builder.trait_impl (eq, std::move (peq_generics.self_type),
				 std::move (trait_items),
				 std::move (peq_generics.impl));

  auto structural_peq
    = builder.trait_impl (speq, std::move (speq_generics.self_type), {},
			  std::move (speq_generics.impl));

  return vec (std::move (peq), std::move (structural_peq));
}

std::unique_ptr<AssociatedItem>
DerivePartialEq::eq_fn (std::unique_ptr<Expr> &&cmp_expression,
			std::string type_name)
{
  auto block = builder.block (tl::nullopt, std::move (cmp_expression));

  auto self_type
    = std::unique_ptr<TypeNoBounds> (new TypePath (builder.type_path ("Self")));

  auto params
    = vec (builder.self_ref_param (),
	   builder.function_param (builder.identifier_pattern ("other"),
				   builder.reference_type (
				     std::move (self_type))));

  return builder.function ("eq", std::move (params),
			   builder.single_type_path ("bool"),
			   std::move (block));
}

std::unique_ptr<Expr>
DerivePartialEq::build_eq_expression (
  std::vector<SelfOther> &&field_expressions)
{
  // for unit structs or empty tuples, this is always true
  if (field_expressions.empty ())
    return builder.literal_bool (true);

  auto cmp_expression
    = builder.comparison_expr (std::move (field_expressions.at (0).self_expr),
			       std::move (field_expressions.at (0).other_expr),
			       ComparisonOperator::EQUAL);

  for (size_t i = 1; i < field_expressions.size (); i++)
    {
      auto tmp = builder.comparison_expr (
	std::move (field_expressions.at (i).self_expr),
	std::move (field_expressions.at (i).other_expr),
	ComparisonOperator::EQUAL);

      cmp_expression
	= builder.boolean_operation (std::move (cmp_expression),
				     std::move (tmp),
				     LazyBooleanOperator::LOGICAL_AND);
    }

  return cmp_expression;
}

void
DerivePartialEq::visit_tuple (TupleStruct &item)
{
  auto type_name = item.get_struct_name ().as_string ();
  auto fields = std::vector<SelfOther> ();

  for (size_t idx = 0; idx < item.get_fields ().size (); idx++)
    fields.emplace_back (SelfOther::index (builder, idx));

  auto fn = eq_fn (build_eq_expression (std::move (fields)), type_name);

  expanded
    = partialeq_impls (std::move (fn), type_name, item.get_generic_params ());
}

void
DerivePartialEq::visit_struct (StructStruct &item)
{
  auto type_name = item.get_struct_name ().as_string ();
  auto fields = std::vector<SelfOther> ();

  for (auto &field : item.get_fields ())
    fields.emplace_back (
      SelfOther::field (builder, field.get_field_name ().as_string ()));

  auto fn = eq_fn (build_eq_expression (std::move (fields)), type_name);

  expanded
    = partialeq_impls (std::move (fn), type_name, item.get_generic_params ());
}

MatchCase
DerivePartialEq::match_enum_identifier (
  PathInExpression variant_path, const std::unique_ptr<EnumItem> &variant)
{
  auto inner_ref_patterns
    = vec (builder.ref_pattern (
	     std::unique_ptr<Pattern> (new PathInExpression (variant_path))),
	   builder.ref_pattern (
	     std::unique_ptr<Pattern> (new PathInExpression (variant_path))));

  auto tuple_items = std::make_unique<TuplePatternItemsMultiple> (
    std::move (inner_ref_patterns));

  auto pattern = std::make_unique<TuplePattern> (std::move (tuple_items), loc);

  return builder.match_case (std::move (pattern), builder.literal_bool (true));
}

MatchCase
DerivePartialEq::match_enum_tuple (PathInExpression variant_path,
				   const EnumItemTuple &variant)
{
  auto self_patterns = std::vector<std::unique_ptr<Pattern>> ();
  auto other_patterns = std::vector<std::unique_ptr<Pattern>> ();

  auto self_other_exprs = std::vector<SelfOther> ();

  for (size_t i = 0; i < variant.get_tuple_fields ().size (); i++)
    {
      // The patterns we're creating for each field are `self_<i>` and
      // `other_<i>` where `i` is the index of the field. It doesn't actually
      // matter what we use, as long as it's ordered, unique, and that we can
      // reuse it in the match case's return expression to check that they are
      // equal.

      auto self_pattern_str = "__self_" + std::to_string (i);
      auto other_pattern_str = "__other_" + std::to_string (i);

      self_patterns.emplace_back (
	builder.identifier_pattern (self_pattern_str));
      other_patterns.emplace_back (
	builder.identifier_pattern (other_pattern_str));

      self_other_exprs.emplace_back (SelfOther{
	builder.identifier (self_pattern_str),
	builder.identifier (other_pattern_str),
      });
    }

  auto self_pattern_items = std::unique_ptr<TupleStructItems> (
    new TupleStructItemsNoRange (std::move (self_patterns)));
  auto other_pattern_items = std::unique_ptr<TupleStructItems> (
    new TupleStructItemsNoRange (std::move (other_patterns)));

  auto self_pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new TupleStructPattern (
			    variant_path, std::move (self_pattern_items))),
			  false, false, loc));
  auto other_pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new TupleStructPattern (
			    variant_path, std::move (other_pattern_items))),
			  false, false, loc));

  auto tuple_items = std::make_unique<TuplePatternItemsMultiple> (
    vec (std::move (self_pattern), std::move (other_pattern)));

  auto pattern = std::make_unique<TuplePattern> (std::move (tuple_items), loc);

  auto expr = build_eq_expression (std::move (self_other_exprs));

  return builder.match_case (std::move (pattern), std::move (expr));
}

MatchCase
DerivePartialEq::match_enum_struct (PathInExpression variant_path,
				    const EnumItemStruct &variant)
{
  auto self_fields = std::vector<std::unique_ptr<StructPatternField>> ();
  auto other_fields = std::vector<std::unique_ptr<StructPatternField>> ();

  auto self_other_exprs = std::vector<SelfOther> ();

  for (auto &field : variant.get_struct_fields ())
    {
      // The patterns we're creating for each field are `self_<field>` and
      // `other_<field>` where `field` is the name of the field. It doesn't
      // actually matter what we use, as long as it's ordered, unique, and that
      // we can reuse it in the match case's return expression to check that
      // they are equal.

      auto field_name = field.get_field_name ().as_string ();

      auto self_pattern_str = "__self_" + field_name;
      auto other_pattern_str = "__other_" + field_name;

      self_fields.emplace_back (builder.struct_pattern_ident_pattern (
	field_name, builder.identifier_pattern (self_pattern_str)));
      other_fields.emplace_back (builder.struct_pattern_ident_pattern (
	field_name, builder.identifier_pattern (other_pattern_str)));

      self_other_exprs.emplace_back (SelfOther{
	builder.identifier (self_pattern_str),
	builder.identifier (other_pattern_str),
      });
    }

  auto self_elts = StructPatternElements (std::move (self_fields));
  auto other_elts = StructPatternElements (std::move (other_fields));

  auto self_pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new StructPattern (
			    variant_path, loc, std::move (self_elts))),
			  false, false, loc));
  auto other_pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new StructPattern (
			    variant_path, loc, std::move (other_elts))),
			  false, false, loc));

  auto tuple_items = std::make_unique<TuplePatternItemsMultiple> (
    vec (std::move (self_pattern), std::move (other_pattern)));

  auto pattern = std::make_unique<TuplePattern> (std::move (tuple_items), loc);

  auto expr = build_eq_expression (std::move (self_other_exprs));

  return builder.match_case (std::move (pattern), std::move (expr));
}

void
DerivePartialEq::visit_enum (Enum &item)
{
  auto cases = std::vector<MatchCase> ();
  auto type_name = item.get_identifier ().as_string ();

  for (auto &variant : item.get_variants ())
    {
      auto variant_path
	= builder.variant_path (type_name,
				variant->get_identifier ().as_string ());

      switch (variant->get_enum_item_kind ())
	{
	case EnumItem::Kind::Identifier:
	case EnumItem::Kind::Discriminant:
	  cases.emplace_back (match_enum_identifier (variant_path, variant));
	  break;
	case EnumItem::Kind::Tuple:
	  cases.emplace_back (
	    match_enum_tuple (variant_path,
			      static_cast<EnumItemTuple &> (*variant)));
	  break;
	case EnumItem::Kind::Struct:
	  cases.emplace_back (
	    match_enum_struct (variant_path,
			       static_cast<EnumItemStruct &> (*variant)));
	  break;
	}
    }

  // NOTE: Mention using discriminant_value and skipping that last case, and
  // instead skipping all identifiers/discriminant enum items and returning
  // `true` in the wildcard case

  // In case the two instances of `Self` don't have the same discriminant,
  // automatically return false.
  cases.emplace_back (
    builder.match_case (builder.wildcard (), builder.literal_bool (false)));

  auto match
    = builder.match (builder.tuple (vec (builder.identifier ("self"),
					 builder.identifier ("other"))),
		     std::move (cases));

  auto fn = eq_fn (std::move (match), type_name);

  expanded
    = partialeq_impls (std::move (fn), type_name, item.get_generic_params ());
}

void
DerivePartialEq::visit_union (Union &item)
{
  rust_error_at (item.get_locus (),
		 "derive(PartialEq) cannot be used on unions");
}

} // namespace AST
} // namespace Rust
