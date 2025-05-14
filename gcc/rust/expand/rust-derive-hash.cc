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

#include "rust-derive-hash.h"
#include "rust-ast.h"
#include "rust-expr.h"
#include "rust-item.h"
#include "rust-path.h"
#include "rust-pattern.h"
#include "rust-stmt.h"
#include "rust-system.h"

namespace Rust {
namespace AST {

DeriveHash::DeriveHash (location_t loc) : DeriveVisitor (loc) {}

std::unique_ptr<AST::Item>
DeriveHash::go (Item &item)
{
  item.accept_vis (*this);

  return std::move (expanded);
}

std::unique_ptr<Expr>
DeriveHash::hash_call (std::unique_ptr<Expr> &&value)
{
  auto hash
    = builder.path_in_expression ({"core", "hash", "Hash", "hash"}, true);

  return builder.call (ptrify (hash),
		       vec (std::move (value),
			    builder.identifier (DeriveHash::state)));
}

std::unique_ptr<AssociatedItem>
DeriveHash::hash_fn (std::unique_ptr<BlockExpr> &&block)
{
  auto hash_calls = std::vector<std::unique_ptr<Stmt>> ();

  auto state_type = std::unique_ptr<TypeNoBounds> (
    new TypePath (builder.type_path (DeriveHash::state_type)));
  auto state_param =

    builder.function_param (builder.identifier_pattern (DeriveHash::state),
			    builder.reference_type (std::move (state_type),
						    true));

  auto params = vec (builder.self_ref_param (), std::move (state_param));
  auto bounds = vec (
    builder.trait_bound (builder.type_path ({"core", "hash", "Hasher"}, true)));
  auto generics = vec (
    builder.generic_type_param (DeriveHash::state_type, std::move (bounds)));

  return builder.function ("hash", std::move (params), nullptr,
			   std::move (block), std::move (generics));
}

std::unique_ptr<Item>
DeriveHash::hash_impl (
  std::unique_ptr<AssociatedItem> &&hash_fn, std::string name,
  const std::vector<std::unique_ptr<GenericParam>> &type_generics)
{
  auto hash_path = builder.type_path ({"core", "hash", "Hash"}, true);

  auto trait_items = vec (std::move (hash_fn));

  auto generics = setup_impl_generics (name, type_generics,
				       builder.trait_bound (hash_path));

  return builder.trait_impl (hash_path, std::move (generics.self_type),
			     std::move (trait_items),
			     std::move (generics.impl));
}

void
DeriveHash::visit_struct (StructStruct &item)
{
  auto hash_calls = std::vector<std::unique_ptr<Stmt>> ();

  for (auto &field : item.get_fields ())
    {
      auto value = builder.ref (
	builder.field_access (builder.identifier ("self"),
			      field.get_field_name ().as_string ()));

      auto stmt = builder.statementify (hash_call (std::move (value)));

      hash_calls.emplace_back (std::move (stmt));
    }

  auto block = builder.block (std::move (hash_calls));

  expanded = hash_impl (hash_fn (std::move (block)),
			item.get_identifier ().as_string (),
			item.get_generic_params ());
}

void
DeriveHash::visit_tuple (TupleStruct &item)
{
  auto hash_calls = std::vector<std::unique_ptr<Stmt>> ();

  for (size_t idx = 0; idx < item.get_fields ().size (); idx++)
    {
      auto value = builder.ref (builder.tuple_idx ("self", idx));

      auto stmt = builder.statementify (hash_call (std::move (value)));

      hash_calls.emplace_back (std::move (stmt));
    }

  auto block = builder.block (std::move (hash_calls));

  expanded = hash_impl (hash_fn (std::move (block)),
			item.get_identifier ().as_string (),
			item.get_generic_params ());
}

MatchCase
DeriveHash::match_enum_tuple (PathInExpression variant_path,
			      const EnumItemTuple &variant)
{
  auto self_patterns = std::vector<std::unique_ptr<Pattern>> ();
  auto hash_calls = std::vector<std::unique_ptr<Stmt>> ();

  for (size_t i = 0; i < variant.get_tuple_fields ().size (); i++)
    {
      auto pattern = "__self_" + std::to_string (i);

      auto call = hash_call (builder.ref (builder.identifier (pattern)));

      self_patterns.emplace_back (builder.identifier_pattern (pattern));
      hash_calls.emplace_back (builder.statementify (std::move (call)));
    }

  auto patterns_elts = std::unique_ptr<TupleStructItems> (
    new TupleStructItemsNoRange (std::move (self_patterns)));
  auto pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new TupleStructPattern (
			    variant_path, std::move (patterns_elts))),
			  false, false, loc));

  auto block = builder.block (std::move (hash_calls));

  return builder.match_case (std::move (pattern), std::move (block));
}

MatchCase
DeriveHash::match_enum_struct (PathInExpression variant_path,
			       const EnumItemStruct &variant)
{
  auto field_patterns = std::vector<std::unique_ptr<StructPatternField>> ();
  auto hash_calls = std::vector<std::unique_ptr<Stmt>> ();

  for (const auto &field : variant.get_struct_fields ())
    {
      auto call = hash_call (builder.ref (
	builder.identifier (field.get_field_name ().as_string ())));

      field_patterns.emplace_back (
	std::unique_ptr<StructPatternField> (new StructPatternFieldIdent (
	  field.get_field_name (), false /* is_ref? true? */, false, {}, loc)));

      hash_calls.emplace_back (builder.statementify (std::move (call)));
    }

  auto pattern_elts = StructPatternElements (std::move (field_patterns));
  auto pattern = std::unique_ptr<Pattern> (
    new ReferencePattern (std::unique_ptr<Pattern> (new StructPattern (
			    variant_path, loc, pattern_elts)),
			  false, false, loc));

  auto block = builder.block (std::move (hash_calls));
  return builder.match_case (std::move (pattern), std::move (block));
}

void
DeriveHash::visit_enum (Enum &item)
{
  // Enums are a bit different: We start by hashing the discriminant value of
  // the enum instance, and then hash all of the data contained in each of the
  // enum's variants. For data-less variants, we don't have any data to hash, so
  // hashing the discriminant value is enough. To access the rest of the
  // variants' data, we create a match and destructure each internal field and
  // hash it.
  //
  // So for example with the following enum:
  //
  // ```rust
  // enum Foo {
  //     A,
  //     B(i32),
  //     C { a: i32 },
  // }
  // ```
  //
  // we create the following implementation:
  //
  // ```rust
  // fn hash<H: Hasher>(&self, state: &mut H) {
  //     let discriminant = intrinsics::discriminant_value(&self);
  //     Hash::hash(&discriminant, state);
  //
  //     match self {
  //         B(self_0) => { Hash::hash(self_0, state); },
  //         C { a } => { Hash::hash(a, state); },
  //         _ => {},
  //     }
  // }
  // ```
  //
  // Note the extra wildcard pattern to satisfy the exhaust checker.

  auto cases = std::vector<MatchCase> ();
  auto type_name = item.get_identifier ().as_string ();

  auto intrinsic = ptrify (
    builder.path_in_expression ({"core", "intrinsics", "discriminant_value"},
				true));

  auto let_discr
    = builder.let (builder.identifier_pattern (DeriveHash::discr), nullptr,
		   builder.call (std::move (intrinsic),
				 builder.identifier ("self")));

  auto discr_hash = builder.statementify (
    hash_call (builder.ref (builder.identifier (DeriveHash::discr))));

  for (auto &variant : item.get_variants ())
    {
      auto variant_path
	= builder.variant_path (type_name,
				variant->get_identifier ().as_string ());

      switch (variant->get_enum_item_kind ())
	{
	case EnumItem::Kind::Identifier:
	case EnumItem::Kind::Discriminant:
	  // nothing to do in these cases, as we just need to hash the
	  // discriminant value
	  continue;
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

  // The extra empty wildcard case
  cases.emplace_back (
    builder.match_case (builder.wildcard (), builder.block ()));

  auto match = builder.match (builder.identifier ("self"), std::move (cases));

  auto block
    = builder.block (vec (std::move (let_discr), std::move (discr_hash)),
		     std::move (match));

  expanded = hash_impl (hash_fn (std::move (block)), type_name,
			item.get_generic_params ());
}

void
DeriveHash::visit_union (Union &item)
{
  rust_error_at (item.get_locus (), "derive(Hash) cannot be used on unions");
}

} // namespace AST
} // namespace Rust
