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

#ifndef RUST_DERIVE_EQ_H
#define RUST_DERIVE_EQ_H

#include "rust-derive.h"

namespace Rust {
namespace AST {

// FIXME: Need to figure out structuraleq marker trait

class DeriveEq : DeriveVisitor
{
public:
  DeriveEq (location_t loc);

  std::vector<std::unique_ptr<AST::Item>> go (Item &item);

private:
  std::vector<std::unique_ptr<Item>> expanded;

  /**
   * Create the actual `assert_receiver_is_total_eq` function of the
   * implementation, which asserts that every type contained within our targeted
   * type also implements `Eq`.
   */
  std::unique_ptr<AssociatedItem>
  assert_receiver_is_total_eq_fn (std::vector<std::unique_ptr<Type>> &&types);

  /**
   * Create the Eq trait implementation for a type
   *
   * impl Eq for <type> {
   *     <assert_receiver_is_total_eq>
   * }
   *
   */
  std::vector<std::unique_ptr<Item>>
  eq_impls (std::unique_ptr<AssociatedItem> &&fn, std::string name,
	    const std::vector<std::unique_ptr<GenericParam>> &type_generics);

  /**
   * Generate the following structure definition
   *
   * struct AssertParamIsEq<T: Eq + ?Sized> { _t: PhantomData<T> }
   */
  std::unique_ptr<Stmt> assert_param_is_eq ();

  /**
   * Generate a let statement to assert a type implements `Eq`
   *
   * let _: AssertParamIsEq<type>;
   */
  std::unique_ptr<Stmt> assert_type_is_eq (std::unique_ptr<Type> &&type);

  virtual void visit_struct (StructStruct &item);
  virtual void visit_tuple (TupleStruct &item);
  virtual void visit_enum (Enum &item);
  virtual void visit_union (Union &item);
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_DERIVE_EQ_H
