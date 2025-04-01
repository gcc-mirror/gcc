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

#include "rust-derive-clone.h"
#include "rust-ast.h"
#include "rust-expr.h"
#include "rust-item.h"
#include "rust-path.h"
#include "rust-pattern.h"
#include "rust-system.h"

namespace Rust {
namespace AST {

std::unique_ptr<Expr>
DeriveClone::clone_call (std::unique_ptr<Expr> &&to_clone)
{
  // $crate::core::clone::Clone::clone for the fully qualified path - we don't
  // link with `core` yet so that might be an issue. Use `Clone::clone` for now?
  // TODO: Factor this function inside the DeriveAccumulator

  // Interestingly, later versions of Rust have a `clone_fn` lang item which
  // corresponds to this. But because we are first targeting 1.49, we cannot use
  // it yet. Once we target a new, more recent version of the language, we'll
  // have figured out how to compile and distribute `core`, meaning we'll be
  // able to directly call `::core::clone::Clone::clone()`

  // Not sure how to call it properly in the meantime...

  auto path = std::unique_ptr<Expr> (
    new PathInExpression (builder.path_in_expression ({"Clone", "clone"})));

  auto args = std::vector<std::unique_ptr<Expr>> ();
  args.emplace_back (std::move (to_clone));

  return builder.call (std::move (path), std::move (args));
}

/**
 * Create the actual "clone" function of the implementation, so
 *
 * fn clone(&self) -> Self { <clone_expr> }
 *
 */
std::unique_ptr<AssociatedItem>
DeriveClone::clone_fn (std::unique_ptr<Expr> &&clone_expr)
{
  auto block = std::unique_ptr<BlockExpr> (
    new BlockExpr ({}, std::move (clone_expr), {}, {}, tl::nullopt, loc, loc));
  auto big_self_type = builder.single_type_path ("Self");

  std::unique_ptr<SelfParam> self (new SelfParam (tl::nullopt,
						  /* is_mut */ false, loc));

  std::vector<std::unique_ptr<Param>> params;
  params.push_back (std::move (self));

  return std::unique_ptr<AssociatedItem> (
    new Function ({"clone"}, builder.fn_qualifiers (), /* generics */ {},
		  /* function params */ std::move (params),
		  std::move (big_self_type), WhereClause::create_empty (),
		  std::move (block), Visibility::create_private (), {}, loc));
}

/**
 * Create the Clone trait implementation for a type
 *
 * impl Clone for <type> {
 *     <clone_fn>
 * }
 *
 */
std::unique_ptr<Item>
DeriveClone::clone_impl (
  std::unique_ptr<AssociatedItem> &&clone_fn, std::string name,
  const std::vector<std::unique_ptr<GenericParam>> &type_generics)
{
  // we should have two of these, so we don't run into issues with
  // two paths sharing a node id
  auto clone_bound = builder.type_path (LangItem::Kind::CLONE);
  auto clone_trait_path = builder.type_path (LangItem::Kind::CLONE);

  auto trait_items = vec (std::move (clone_fn));

  auto generics = setup_impl_generics (name, type_generics,
				       builder.trait_bound (clone_bound));

  return builder.trait_impl (clone_trait_path, std::move (generics.self_type),
			     std::move (trait_items),
			     std::move (generics.impl));
}

// TODO: Create new `make_qualified_call` helper function

DeriveClone::DeriveClone (location_t loc)
  : DeriveVisitor (loc), expanded (nullptr)
{}

std::unique_ptr<AST::Item>
DeriveClone::go (Item &item)
{
  item.accept_vis (*this);

  rust_assert (expanded);

  return std::move (expanded);
}

void
DeriveClone::visit_tuple (TupleStruct &item)
{
  auto cloned_fields = std::vector<std::unique_ptr<Expr>> ();

  for (size_t idx = 0; idx < item.get_fields ().size (); idx++)
    cloned_fields.emplace_back (
      clone_call (builder.ref (builder.tuple_idx ("self", idx))));

  auto path = std::unique_ptr<Expr> (new PathInExpression (
    builder.path_in_expression ({item.get_identifier ().as_string ()})));
  auto constructor = builder.call (std::move (path), std::move (cloned_fields));

  expanded = clone_impl (clone_fn (std::move (constructor)),
			 item.get_identifier ().as_string (),
			 item.get_generic_params ());
}

void
DeriveClone::visit_struct (StructStruct &item)
{
  if (item.is_unit_struct ())
    {
      auto unit_ctor
	= builder.struct_expr_struct (item.get_struct_name ().as_string ());
      expanded = clone_impl (clone_fn (std::move (unit_ctor)),
			     item.get_struct_name ().as_string (),
			     item.get_generic_params ());
      return;
    }

  auto cloned_fields = std::vector<std::unique_ptr<StructExprField>> ();
  for (auto &field : item.get_fields ())
    {
      auto name = field.get_field_name ().as_string ();
      auto expr = clone_call (
	builder.ref (builder.field_access (builder.identifier ("self"), name)));

      cloned_fields.emplace_back (
	builder.struct_expr_field (std::move (name), std::move (expr)));
    }

  auto ctor = builder.struct_expr (item.get_struct_name ().as_string (),
				   std::move (cloned_fields));
  expanded = clone_impl (clone_fn (std::move (ctor)),
			 item.get_struct_name ().as_string (),
			 item.get_generic_params ());
}

MatchCase
DeriveClone::clone_enum_identifier (PathInExpression variant_path,
				    const std::unique_ptr<EnumItem> &variant)
{
  auto pattern = std::unique_ptr<Pattern> (new ReferencePattern (
    std::unique_ptr<Pattern> (new PathInExpression (
      variant_path.get_segments (), {}, variant_path.get_locus (),
      variant_path.opening_scope_resolution ())),
    false, false, loc));
  auto expr = std::unique_ptr<Expr> (
    new PathInExpression (variant_path.get_segments (), {},
			  variant_path.get_locus (),
			  variant_path.opening_scope_resolution ()));

  return builder.match_case (std::move (pattern), std::move (expr));
}

MatchCase
DeriveClone::clone_enum_tuple (PathInExpression variant_path,
			       const EnumItemTuple &variant)
{
  auto patterns = std::vector<std::unique_ptr<Pattern>> ();
  auto cloned_patterns = std::vector<std::unique_ptr<Expr>> ();

  for (size_t i = 0; i < variant.get_tuple_fields ().size (); i++)
    {
      // The pattern we're creating for each field is `self_<i>` where `i` is
      // the index of the field. It doesn't actually matter what we use, as long
      // as it's ordered, unique, and that we can reuse it in the match case's
      // return expression to clone the field.
      auto pattern_str = "__self_" + std::to_string (i);

      patterns.emplace_back (builder.identifier_pattern (pattern_str));

      // Now, for each tuple's element, we create a new expression calling
      // `clone` on it for the match case's return expression
      cloned_patterns.emplace_back (
	clone_call (builder.ref (builder.identifier (pattern_str))));
    }

  auto pattern_items = std::unique_ptr<TupleStructItems> (
    new TupleStructItemsNoRange (std::move (patterns)));

  auto pattern = std::unique_ptr<Pattern> (new ReferencePattern (
    std::unique_ptr<Pattern> (new TupleStructPattern (
      PathInExpression (variant_path.get_segments (), {},
			variant_path.get_locus (),
			variant_path.opening_scope_resolution ()),
      std::move (pattern_items))),
    false, false, loc));

  auto expr = builder.call (std::unique_ptr<Expr> (new PathInExpression (
			      variant_path.get_segments (), {},
			      variant_path.get_locus (),
			      variant_path.opening_scope_resolution ())),
			    std::move (cloned_patterns));

  return builder.match_case (std::move (pattern), std::move (expr));
}

MatchCase
DeriveClone::clone_enum_struct (PathInExpression variant_path,
				const EnumItemStruct &variant)
{
  auto field_patterns = std::vector<std::unique_ptr<StructPatternField>> ();
  auto cloned_fields = std::vector<std::unique_ptr<StructExprField>> ();

#if 0
  // NOTE: We currently do not support compiling struct patterns where an
  // identifier is assigned a new pattern, e.g. Bloop { f0: x }
  // This is the code we should eventually produce as it mimics what rustc does
  // - which is probably here for a good reason. In the meantime, we can just
  // use the field's identifier as the pattern: Bloop { f0 }
  // We can then clone the field directly instead of calling `clone()` on the
  // new pattern.
  // TODO: Figure out if that is actually needed and why rustc does it?

  for (size_t i = 0; i < variant.get_struct_fields ().size (); i++)
    {
      auto &field = variant.get_struct_fields ()[i];

      // Just like for tuples, the pattern we're creating for each field is
      // `self_<i>` where `i` is the index of the field. It doesn't actually
      // matter what we use, as long as it's ordered, unique, and that we can
      // reuse it in the match case's return expression to clone the field.
      auto pattern_str = "__self_" + std::to_string (i);

      field_patterns.emplace_back (
	std::unique_ptr<StructPatternField> (new StructPatternFieldIdentPat (
	  field.get_field_name (), builder.identifier_pattern (pattern_str), {},
	  loc)));

      cloned_fields.emplace_back (
	std::unique_ptr<StructExprField> (new StructExprFieldIdentifierValue (
	  field.get_field_name (),
	  clone_call (builder.ref (builder.identifier (pattern_str))), {},
	  loc)));
    }
#endif

  for (const auto &field : variant.get_struct_fields ())
    {
      // We match on the struct's fields, and then recreate an instance of that
      // struct, cloning each field

      field_patterns.emplace_back (
	std::unique_ptr<StructPatternField> (new StructPatternFieldIdent (
	  field.get_field_name (), false /* is_ref? true? */, false, {}, loc)));

      cloned_fields.emplace_back (
	std::unique_ptr<StructExprField> (new StructExprFieldIdentifierValue (
	  field.get_field_name (),
	  clone_call (builder.ref (
	    builder.identifier (field.get_field_name ().as_string ()))),
	  {}, loc)));
    }

  auto pattern_elts = StructPatternElements (std::move (field_patterns));

  auto pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new StructPattern (
			    variant_path, loc, pattern_elts)),
			  false, false, loc));
  auto expr = std::unique_ptr<Expr> (
    new StructExprStructFields (variant_path, std::move (cloned_fields), loc));

  return builder.match_case (std::move (pattern), std::move (expr));
}

void
DeriveClone::visit_enum (Enum &item)
{
  // Create an arm for each variant of the enum:
  // - For enum item variants (simple identifiers), just create the same
  // variant.
  // - For struct and tuple variants, destructure the pattern and call clone for
  // each field.

  auto cases = std::vector<MatchCase> ();

  for (const auto &variant : item.get_variants ())
    {
      auto path
	= builder.variant_path (item.get_identifier ().as_string (),
				variant->get_identifier ().as_string ());

      switch (variant->get_enum_item_kind ())
	{
	// Identifiers and discriminated variants are the same for a clone - we
	// just return the same variant
	case EnumItem::Kind::Identifier:
	case EnumItem::Kind::Discriminant:
	  cases.emplace_back (clone_enum_identifier (path, variant));
	  break;
	case EnumItem::Kind::Tuple:
	  cases.emplace_back (
	    clone_enum_tuple (path, static_cast<EnumItemTuple &> (*variant)));
	  break;
	case EnumItem::Kind::Struct:
	  cases.emplace_back (
	    clone_enum_struct (path, static_cast<EnumItemStruct &> (*variant)));
	  break;
	}
    }

  // match self { ... }
  auto match = builder.match (builder.identifier ("self"), std::move (cases));

  expanded = clone_impl (clone_fn (std::move (match)),
			 item.get_identifier ().as_string (),
			 item.get_generic_params ());
}

void
DeriveClone::visit_union (Union &item)
{
  // FIXME: Should be $crate::core::clone::AssertParamIsCopy (or similar)
  // (Rust-GCC#3329)

  auto copy_path = builder.type_path (LangItem::Kind::COPY);
  auto sized_path = builder.type_path (LangItem::Kind::SIZED);

  auto copy_bound = std::unique_ptr<TypeParamBound> (
    new TraitBound (copy_path, item.get_locus ()));
  auto sized_bound = std::unique_ptr<TypeParamBound> (
    new TraitBound (sized_path, item.get_locus (), false,
		    true /* opening_question_mark */));

  auto bounds = vec (std::move (copy_bound), std::move (sized_bound));

  // struct AssertParamIsCopy<T: Copy + ?Sized> { _t: PhantomData<T> }
  auto assert_param_is_copy = "AssertParamIsCopy";
  auto t = std::unique_ptr<GenericParam> (
    new TypeParam (Identifier ("T"), item.get_locus (), std::move (bounds)));
  auto assert_param_is_copy_struct = builder.struct_struct (
    assert_param_is_copy, vec (std::move (t)),
    {StructField (
      Identifier ("_t"),
      builder.single_generic_type_path (
	LangItem::Kind::PHANTOM_DATA,
	GenericArgs (
	  {}, {GenericArg::create_type (builder.single_type_path ("T"))}, {})),
      Visibility::create_private (), item.get_locus ())});

  // <Self>
  auto arg = GenericArg::create_type (builder.single_type_path ("Self"));

  // AssertParamIsCopy::<Self>
  auto type = std::unique_ptr<TypePathSegment> (
    new TypePathSegmentGeneric (PathIdentSegment (assert_param_is_copy, loc),
				false, GenericArgs ({}, {arg}, {}, loc), loc));
  auto type_paths = std::vector<std::unique_ptr<TypePathSegment>> ();
  type_paths.emplace_back (std::move (type));

  auto full_path
    = std::unique_ptr<Type> (new TypePath ({std::move (type_paths)}, loc));

  auto tail_expr = builder.deref (builder.identifier ("self"));

  auto stmts
    = vec (std::move (assert_param_is_copy_struct),
	   builder.let (builder.wildcard (), std::move (full_path), nullptr));

  auto block = builder.block (std::move (stmts), std::move (tail_expr));

  expanded = clone_impl (clone_fn (std::move (block)),
			 item.get_identifier ().as_string (),
			 item.get_generic_params ());
}

} // namespace AST
} // namespace Rust
