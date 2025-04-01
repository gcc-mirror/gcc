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

#include "rust-derive-debug.h"
#include "rust-ast.h"
#include "rust-hir-map.h"
#include "rust-system.h"

namespace Rust {
namespace AST {

DeriveDebug::DeriveDebug (location_t loc)
  : DeriveVisitor (loc), expanded (nullptr)
{}

std::unique_ptr<Item>
DeriveDebug::go (Item &item)
{
  item.accept_vis (*this);

  rust_assert (expanded);

  return std::move (expanded);
}

std::unique_ptr<AssociatedItem>
DeriveDebug::stub_debug_fn ()
{
  auto unit_expr = builder.tuple ();
  auto ok_expr
    = ptrify (builder.path_in_expression (LangItem::Kind::RESULT_OK));

  auto stub_return = builder.call (std::move (ok_expr), std::move (unit_expr));

  // we can't use builder.block() here as it returns a unique_ptr<Expr> and
  // Function's constructor expects a unique_ptr<BlockExpr>
  auto block = std::unique_ptr<BlockExpr> (
    new BlockExpr ({}, std::move (stub_return), {}, {}, tl::nullopt, loc, loc));

  auto self = builder.self_ref_param ();

  auto return_type
    = ptrify (builder.type_path ({"core", "fmt", "Result"}, true));

  auto mut_fmt_type_inner
    = ptrify (builder.type_path ({"core", "fmt", "Formatter"}, true));

  auto mut_fmt_type
    = builder.reference_type (std::move (mut_fmt_type_inner), true);

  auto fmt = builder.function_param (builder.identifier_pattern ("_fmt"),
				     std::move (mut_fmt_type));

  auto params = vec (std::move (self), std::move (fmt));

  auto function = builder.function ("fmt", std::move (params),
				    std::move (return_type), std::move (block));

  return function;
}

std::unique_ptr<Item>
DeriveDebug::stub_derive_impl (
  std::string name,
  const std::vector<std::unique_ptr<GenericParam>> &type_generics)
{
  auto trait_items = vec (stub_debug_fn ());

  auto debug = builder.type_path ({"core", "fmt", "Debug"}, true);
  auto generics
    = setup_impl_generics (name, type_generics, builder.trait_bound (debug));

  return builder.trait_impl (debug, std::move (generics.self_type),
			     std::move (trait_items),
			     std::move (generics.impl));
}

void
DeriveDebug::visit_struct (StructStruct &struct_item)
{
  expanded = stub_derive_impl (struct_item.get_identifier ().as_string (),
			       struct_item.get_generic_params ());
}

void
DeriveDebug::visit_tuple (TupleStruct &tuple_item)
{
  expanded = stub_derive_impl (tuple_item.get_identifier ().as_string (),
			       tuple_item.get_generic_params ());
}

void
DeriveDebug::visit_enum (Enum &enum_item)
{
  expanded = stub_derive_impl (enum_item.get_identifier ().as_string (),
			       enum_item.get_generic_params ());
}

void
DeriveDebug::visit_union (Union &enum_item)
{
  rust_error_at (loc, "derive(Debug) cannot be derived for unions");
}

} // namespace AST
} // namespace Rust
