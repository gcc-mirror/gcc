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

#ifndef RUST_DERIVE_COPY_H
#define RUST_DERIVE_COPY_H

#include "rust-derive.h"
#include "rust-ast-builder.h"

namespace Rust {
namespace AST {
class DeriveCopy : DeriveVisitor
{
public:
  DeriveCopy (location_t loc);

  std::unique_ptr<Item> go (Item &);

private:
  std::unique_ptr<Item> expanded;

  /**
   * Create the Copy impl block for a type. These impl blocks are very simple as
   * Copy is just a marker trait.
   *
   * impl Copy for <type> {}
   */
  std::unique_ptr<Item>
  copy_impl (std::string name,
	     const std::vector<std::unique_ptr<GenericParam>> &type_generics);

  virtual void visit_struct (StructStruct &item);
  virtual void visit_tuple (TupleStruct &item);
  virtual void visit_enum (Enum &item);
  virtual void visit_union (Union &item);
};

} // namespace AST
} // namespace Rust

#endif // !RUST_DERIVE_COPY_H
