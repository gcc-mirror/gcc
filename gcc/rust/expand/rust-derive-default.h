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

#ifndef RUST_DERIVE_DEFAULT_H
#define RUST_DERIVE_DEFAULT_H

#include "rust-derive.h"
#include "rust-ast.h"

namespace Rust {
namespace AST {

// This derive is currently incomplete and only generate a stub implementation
// which does not do any debug formatting
class DeriveDefault : DeriveVisitor
{
public:
  DeriveDefault (location_t loc);

  std::unique_ptr<Item> go (Item &);

private:
  std::unique_ptr<Item> expanded;

  std::unique_ptr<Expr> default_call (std::unique_ptr<Type> &&type);

  std::unique_ptr<AssociatedItem>
  default_fn (std::unique_ptr<Expr> &&return_expr);

  std::unique_ptr<Item> default_impl (
    std::unique_ptr<AssociatedItem> &&default_fn, std::string name,
    const std::vector<std::unique_ptr<GenericParam>> &type_generics);

  virtual void visit_struct (StructStruct &struct_item) override;
  virtual void visit_tuple (TupleStruct &tuple_item) override;
  virtual void visit_enum (Enum &enum_item) override;
  virtual void visit_union (Union &enum_item) override;
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_DERIVE_DEFAULT_H
