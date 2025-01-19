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

#include "rust-derive.h"
#include "rust-derive-clone.h"
#include "rust-derive-copy.h"

namespace Rust {
namespace AST {

DeriveVisitor::DeriveVisitor (location_t loc)
  : loc (loc), builder (Builder (loc))
{}

std::unique_ptr<Item>
DeriveVisitor::derive (Item &item, const Attribute &attr,
		       BuiltinMacro to_derive)
{
  switch (to_derive)
    {
    case BuiltinMacro::Clone:
      return DeriveClone (attr.get_locus ()).go (item);
    case BuiltinMacro::Copy:
      return DeriveCopy (attr.get_locus ()).go (item);
    case BuiltinMacro::Debug:
    case BuiltinMacro::Default:
    case BuiltinMacro::Eq:
    case BuiltinMacro::PartialEq:
    case BuiltinMacro::Ord:
    case BuiltinMacro::PartialOrd:
    case BuiltinMacro::Hash:
    default:
      rust_sorry_at (attr.get_locus (), "unimplemented builtin derive macro");
      return nullptr;
    };
}

} // namespace AST
} // namespace Rust
