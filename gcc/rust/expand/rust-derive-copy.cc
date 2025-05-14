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

#include "rust-derive-copy.h"
#include "rust-hir-map.h"
#include "rust-path.h"

namespace Rust {
namespace AST {

DeriveCopy::DeriveCopy (location_t loc)
  : DeriveVisitor (loc), expanded (nullptr)
{}

std::unique_ptr<AST::Item>
DeriveCopy::go (Item &item)
{
  item.accept_vis (*this);

  rust_assert (expanded);

  return std::move (expanded);
}

std::unique_ptr<Item>
DeriveCopy::copy_impl (
  std::string name,
  const std::vector<std::unique_ptr<GenericParam>> &type_generics)
{
  // we should have two of these, so we don't run into issues with
  // two paths sharing a node id
  auto copy_bound = builder.type_path (LangItem::Kind::COPY);
  auto copy_trait_path = builder.type_path (LangItem::Kind::COPY);

  auto generics = setup_impl_generics (name, type_generics,
				       builder.trait_bound (copy_bound));

  return builder.trait_impl (copy_trait_path, std::move (generics.self_type),
			     {}, std::move (generics.impl));
}

void
DeriveCopy::visit_struct (StructStruct &item)
{
  expanded = copy_impl (item.get_struct_name ().as_string (),
			item.get_generic_params ());
}

void
DeriveCopy::visit_tuple (TupleStruct &item)
{
  expanded = copy_impl (item.get_struct_name ().as_string (),
			item.get_generic_params ());
}

void
DeriveCopy::visit_enum (Enum &item)
{
  expanded = copy_impl (item.get_identifier ().as_string (),
			item.get_generic_params ());
}

void
DeriveCopy::visit_union (Union &item)
{
  expanded = copy_impl (item.get_identifier ().as_string (),
			item.get_generic_params ());
}

} // namespace AST
} // namespace Rust
