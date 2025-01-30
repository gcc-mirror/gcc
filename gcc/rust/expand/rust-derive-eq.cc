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

#include "rust-derive-eq.h"
#include "rust-ast.h"
#include "rust-expr.h"
#include "rust-item.h"
#include "rust-path.h"
#include "rust-pattern.h"
#include "rust-system.h"

namespace Rust {
namespace AST {

std::unique_ptr<AssociatedItem>
DeriveEq::assert_receiver_is_total_eq_fn (
  std::vector<std::unique_ptr<Type>> &&types)
{
  auto stmts = std::vector<std::unique_ptr<Stmt>> ();

  stmts.emplace_back (assert_param_is_eq ());

  for (auto &&type : types)
    stmts.emplace_back (assert_type_is_eq (std::move (type)));

  auto block = std::unique_ptr<BlockExpr> (
    new BlockExpr (std::move (stmts), nullptr, {}, {}, AST::LoopLabel::error (),
		   loc, loc));

  auto self = builder.self_ref_param ();

  return builder.function ("assert_receiver_is_total_eq",
			   vec (std::move (self)), {}, std::move (block));
}

std::unique_ptr<Stmt>
DeriveEq::assert_param_is_eq ()
{
  auto eq_bound = std::unique_ptr<TypeParamBound> (
    new TraitBound (builder.type_path ({"core", "cmp", "Eq"}, true), loc));

  auto sized_bound = std::unique_ptr<TypeParamBound> (
    new TraitBound (builder.type_path (LangItem::Kind::SIZED), loc, false,
		    true /* opening_question_mark */));

  auto bounds = vec (std::move (eq_bound), std::move (sized_bound));

  auto assert_param_is_eq = "AssertParamIsEq";

  auto t = std::unique_ptr<GenericParam> (
    new TypeParam (Identifier ("T"), loc, std::move (bounds)));

  return builder.struct_struct (
    assert_param_is_eq, vec (std::move (t)),
    {StructField (
      Identifier ("_t"),
      builder.single_generic_type_path (
	LangItem::Kind::PHANTOM_DATA,
	GenericArgs (
	  {}, {GenericArg::create_type (builder.single_type_path ("T"))}, {})),
      Visibility::create_private (), loc)});
}

std::unique_ptr<Stmt>
DeriveEq::assert_type_is_eq (std::unique_ptr<Type> &&type)
{
  auto assert_param_is_eq = "AssertParamIsEq";

  // AssertParamIsCopy::<Self>
  auto assert_param_is_eq_ty
    = std::unique_ptr<TypePathSegment> (new TypePathSegmentGeneric (
      PathIdentSegment (assert_param_is_eq, loc), false,
      GenericArgs ({}, {GenericArg::create_type (std::move (type))}, {}, loc),
      loc));

  // TODO: Improve this, it's really ugly
  auto type_paths = std::vector<std::unique_ptr<TypePathSegment>> ();
  type_paths.emplace_back (std::move (assert_param_is_eq_ty));

  auto full_path
    = std::unique_ptr<Type> (new TypePath ({std::move (type_paths)}, loc));

  return builder.let (builder.wildcard (), std::move (full_path));
}

std::unique_ptr<Item>
DeriveEq::eq_impl (
  std::unique_ptr<AssociatedItem> &&fn, std::string name,
  const std::vector<std::unique_ptr<GenericParam>> &type_generics)
{
  auto eq = builder.type_path ({"core", "cmp", "Eq"}, true);

  auto trait_items = vec (std::move (fn));

  auto generics
    = setup_impl_generics (name, type_generics, builder.trait_bound (eq));

  return builder.trait_impl (eq, std::move (generics.self_type),
			     std::move (trait_items),
			     std::move (generics.impl));
}

DeriveEq::DeriveEq (location_t loc) : DeriveVisitor (loc), expanded (nullptr) {}

std::unique_ptr<AST::Item>
DeriveEq::go (Item &item)
{
  item.accept_vis (*this);

  rust_assert (expanded);

  return std::move (expanded);
}

void
DeriveEq::visit_tuple (TupleStruct &item)
{
  auto types = std::vector<std::unique_ptr<Type>> ();

  for (auto &field : item.get_fields ())
    types.emplace_back (field.get_field_type ().clone_type ());

  expanded
    = eq_impl (assert_receiver_is_total_eq_fn (std::move (types)),
	       item.get_identifier ().as_string (), item.get_generic_params ());
}

void
DeriveEq::visit_struct (StructStruct &item)
{
  auto types = std::vector<std::unique_ptr<Type>> ();

  for (auto &field : item.get_fields ())
    types.emplace_back (field.get_field_type ().clone_type ());

  expanded
    = eq_impl (assert_receiver_is_total_eq_fn (std::move (types)),
	       item.get_identifier ().as_string (), item.get_generic_params ());
}

void
DeriveEq::visit_enum (Enum &item)
{
  auto types = std::vector<std::unique_ptr<Type>> ();

  for (auto &variant : item.get_variants ())
    {
      switch (variant->get_enum_item_kind ())
	{
	case EnumItem::Kind::Identifier:
	case EnumItem::Kind::Discriminant:
	  // nothing to do as they contain no inner types
	  continue;
	  case EnumItem::Kind::Tuple: {
	    auto tuple = static_cast<EnumItemTuple &> (*variant);

	    for (auto &field : tuple.get_tuple_fields ())
	      types.emplace_back (field.get_field_type ().clone_type ());

	    break;
	  }
	  case EnumItem::Kind::Struct: {
	    auto tuple = static_cast<EnumItemStruct &> (*variant);

	    for (auto &field : tuple.get_struct_fields ())
	      types.emplace_back (field.get_field_type ().clone_type ());

	    break;
	  }
	}
    }

  expanded
    = eq_impl (assert_receiver_is_total_eq_fn (std::move (types)),
	       item.get_identifier ().as_string (), item.get_generic_params ());
}

void
DeriveEq::visit_union (Union &item)
{
  auto types = std::vector<std::unique_ptr<Type>> ();

  for (auto &field : item.get_variants ())
    types.emplace_back (field.get_field_type ().clone_type ());

  expanded
    = eq_impl (assert_receiver_is_total_eq_fn (std::move (types)),
	       item.get_identifier ().as_string (), item.get_generic_params ());
}

} // namespace AST
} // namespace Rust
