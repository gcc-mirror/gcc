// Copyright (C) 2026 Free Software Foundation, Inc.

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

#ifndef RUST_DERIVE_COERCE_POINTEE_H
#define RUST_DERIVE_COERCE_POINTEE_H

#include "rust-derive.h"

namespace Rust {
namespace AST {

class DeriveCoercePointee : DeriveVisitor
{
public:
  DeriveCoercePointee (location_t loc, Builder::Source item_source);

  std::unique_ptr<Item> go (Item &item);

private:
  virtual void visit_struct (StructStruct &item) override {}
  virtual void visit_tuple (TupleStruct &item) override {}
  virtual void visit_enum (Enum &item) override {}
  virtual void visit_union (Union &item) override {}
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_DERIVE_COERCE_POINTEE_H
