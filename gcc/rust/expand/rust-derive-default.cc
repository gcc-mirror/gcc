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

#include "rust-derive-default.h"
#include "rust-ast.h"
#include "rust-diagnostics.h"
#include "rust-path.h"
#include "rust-system.h"

namespace Rust {
namespace AST {

DeriveDefault::DeriveDefault (location_t loc)
  : DeriveVisitor (loc), expanded (nullptr)
{}

std::unique_ptr<Item>
DeriveDefault::go (Item &item)
{
  item.accept_vis (*this);

  rust_assert (expanded);

  return std::move (expanded);
}

std::unique_ptr<Expr>
DeriveDefault::default_call (std::unique_ptr<Type> &&type)
{
  auto default_trait = builder.type_path ({"core", "default", "Default"}, true);

  auto default_fn
    = builder.qualified_path_in_expression (std::move (type), default_trait,
					    builder.path_segment ("default"));

  return builder.call (std::move (default_fn));
}

std::unique_ptr<AssociatedItem>
DeriveDefault::default_fn (std::unique_ptr<Expr> &&return_expr)
{
  auto self_ty
    = std::unique_ptr<Type> (new TypePath (builder.type_path ("Self")));

  auto block = std::unique_ptr<BlockExpr> (
    new BlockExpr ({}, std::move (return_expr), {}, {}, tl::nullopt, loc, loc));

  return builder.function ("default", {}, std::move (self_ty),
			   std::move (block));
}

std::unique_ptr<Item>
DeriveDefault::default_impl (
  std::unique_ptr<AssociatedItem> &&default_fn, std::string name,
  const std::vector<std::unique_ptr<GenericParam>> &type_generics)
{
  auto default_path = builder.type_path ({"core", "default", "Default"}, true);

  auto trait_items = vec (std::move (default_fn));

  auto generics = setup_impl_generics (name, type_generics,
				       builder.trait_bound (default_path));

  return builder.trait_impl (default_path, std::move (generics.self_type),
			     std::move (trait_items),
			     std::move (generics.impl));
}

void
DeriveDefault::visit_struct (StructStruct &item)
{
  if (item.is_unit_struct ())
    {
      auto unit_ctor
	= builder.struct_expr_struct (item.get_struct_name ().as_string ());
      expanded = default_impl (default_fn (std::move (unit_ctor)),
			       item.get_struct_name ().as_string (),
			       item.get_generic_params ());
      return;
    }

  auto cloned_fields = std::vector<std::unique_ptr<StructExprField>> ();
  for (auto &field : item.get_fields ())
    {
      auto name = field.get_field_name ().as_string ();
      auto expr = default_call (field.get_field_type ().clone_type ());

      cloned_fields.emplace_back (
	builder.struct_expr_field (std::move (name), std::move (expr)));
    }

  auto ctor = builder.struct_expr (item.get_struct_name ().as_string (),
				   std::move (cloned_fields));

  expanded = default_impl (default_fn (std::move (ctor)),
			   item.get_struct_name ().as_string (),
			   item.get_generic_params ());
}

void
DeriveDefault::visit_tuple (TupleStruct &tuple_item)
{
  auto defaulted_fields = std::vector<std::unique_ptr<Expr>> ();

  for (auto &field : tuple_item.get_fields ())
    {
      auto type = field.get_field_type ().clone_type ();

      defaulted_fields.emplace_back (default_call (std::move (type)));
    }

  auto return_expr
    = builder.call (builder.identifier (
		      tuple_item.get_struct_name ().as_string ()),
		    std::move (defaulted_fields));

  expanded = default_impl (default_fn (std::move (return_expr)),
			   tuple_item.get_struct_name ().as_string (),
			   tuple_item.get_generic_params ());
}

void
DeriveDefault::visit_enum (Enum &enum_item)
{
  // This is no longer the case in later Rust versions where you can choose a
  // default variant to emit using the `#[default]` attribute:
  //
  // ```rust
  // #[derive(Default)]
  // enum Baz {
  //     #[default]
  //     A,
  //     B(i32),
  //     C { a: i32 }
  // }
  // ```
  //
  // will emit the following impl
  //
  // ```rust
  // impl ::core::default::Default for Baz {
  //     #[inline]
  //     fn default() -> Baz { Self::A }
  // }
  // ```
  rust_error_at (loc, ErrorCode::E0665,
		 "%<Default%> cannot be derived for enums, only structs");
}

void
DeriveDefault::visit_union (Union &enum_item)
{
  rust_error_at (loc, "derive(Default) cannot be used on unions");
}

} // namespace AST
} // namespace Rust
