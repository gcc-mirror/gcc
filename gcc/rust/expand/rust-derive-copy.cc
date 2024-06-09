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

#include "rust-derive-copy.h"
#include "rust-ast-full.h"

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
DeriveCopy::copy_impl (std::string name)
{
  // `$crate::core::marker::Copy` instead
  auto segments = std::vector<std::unique_ptr<TypePathSegment>> ();
  segments.emplace_back (builder.type_path_segment ("Copy"));
  auto copy = TypePath (std::move (segments), loc);

  return std::unique_ptr<Item> (
    new TraitImpl (copy, /* unsafe */ false,
		   /* exclam */ false, /* trait items */ {},
		   /* generics */ {}, builder.single_type_path (name),
		   WhereClause::create_empty (), Visibility::create_private (),
		   {}, {}, loc));
}

void
DeriveCopy::visit_struct (StructStruct &item)
{
  expanded = copy_impl (item.get_struct_name ().as_string ());
}

void
DeriveCopy::visit_tuple (TupleStruct &item)
{
  expanded = copy_impl (item.get_struct_name ().as_string ());
}

void
DeriveCopy::visit_enum (Enum &item)
{
  expanded = copy_impl (item.get_identifier ().as_string ());
}

void
DeriveCopy::visit_union (Union &item)
{
  expanded = copy_impl (item.get_identifier ().as_string ());
}

} // namespace AST
} // namespace Rust
