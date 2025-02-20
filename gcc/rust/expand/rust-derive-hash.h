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

#ifndef RUST_DERIVE_HASH_H
#define RUST_DERIVE_HASH_H

#include "rust-derive.h"

namespace Rust {
namespace AST {

class DeriveHash : DeriveVisitor
{
public:
  DeriveHash (location_t loc);

  std::unique_ptr<AST::Item> go (Item &item);

private:
  std::unique_ptr<Item> expanded;

  constexpr static const char *state = "#state";
  constexpr static const char *state_type = "#__H";
  constexpr static const char *discr = "#discr";

  std::unique_ptr<Expr> hash_call (std::unique_ptr<Expr> &&value);
  std::unique_ptr<AssociatedItem> hash_fn (std::unique_ptr<BlockExpr> &&block);
  std::unique_ptr<Item>
  hash_impl (std::unique_ptr<AssociatedItem> &&hash_fn, std::string name,
	     const std::vector<std::unique_ptr<GenericParam>> &type_generics);

  MatchCase match_enum_tuple (PathInExpression variant_path,
			      const EnumItemTuple &variant);
  MatchCase match_enum_struct (PathInExpression variant_path,
			       const EnumItemStruct &variant);

  virtual void visit_struct (StructStruct &item);
  virtual void visit_tuple (TupleStruct &item);
  virtual void visit_enum (Enum &item);
  virtual void visit_union (Union &item);
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_DERIVE_HASH_H
