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

#ifndef RUST_DERIVE_CLONE_H
#define RUST_DERIVE_CLONE_H

#include "rust-derive.h"

namespace Rust {
namespace AST {

class DeriveClone : DeriveVisitor
{
public:
  DeriveClone (location_t loc);

  std::unique_ptr<AST::Item> go (Item &item);

private:
  std::unique_ptr<Item> expanded;

  /**
   * Create a call to "clone". For now, this creates a call to
   * `Clone::clone`, but should ultimately call into
   * `::core::clone::Clone::clone`
   *
   * Clone::clone(<to_clone>)
   */
  std::unique_ptr<Expr> clone_call (std::unique_ptr<Expr> &&to_clone);

  /**
   * Create the actual "clone" function of the implementation, so
   *
   * fn clone(&self) -> Self { <clone_expr> }
   *
   */
  std::unique_ptr<AssociatedItem> clone_fn (std::unique_ptr<Expr> &&clone_expr);

  /**
   * Create the Clone trait implementation for a type
   *
   * impl Clone for <type> {
   *     <clone_fn>
   * }
   *
   */
  std::unique_ptr<Item> clone_impl (std::unique_ptr<AssociatedItem> &&clone_fn,
				    std::string name);

  virtual void visit_struct (StructStruct &item);
  virtual void visit_tuple (TupleStruct &item);
  virtual void visit_enum (Enum &item);
  virtual void visit_union (Union &item);
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_DERIVE_CLONE_H
