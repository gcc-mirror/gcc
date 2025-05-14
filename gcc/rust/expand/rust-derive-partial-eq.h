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

#ifndef RUST_DERIVE_PARTIAL_EQ_H
#define RUST_DERIVE_PARTIAL_EQ_H

#include "rust-derive.h"
#include "rust-path.h"

namespace Rust {
namespace AST {

class DerivePartialEq : DeriveVisitor
{
public:
  DerivePartialEq (location_t loc);

  std::vector<std::unique_ptr<AST::Item>> go (Item &item);

private:
  std::vector<std::unique_ptr<Item>> expanded;

  /**
   * Generate both an implementation of `PartialEq` and `StructuralPartialEq`
   * for the given type
   */
  std::vector<std::unique_ptr<Item>> partialeq_impls (
    std::unique_ptr<AssociatedItem> &&eq_fn, std::string name,
    const std::vector<std::unique_ptr<GenericParam>> &type_generics);

  std::unique_ptr<AssociatedItem> eq_fn (std::unique_ptr<Expr> &&cmp_expression,
					 std::string type_name);

  /**
   * A pair of two expressions from each instance being compared. E.g. this
   * could be `self.0` and `other.0`, or `self.field` and `other.field`
   */
  struct SelfOther
  {
    std::unique_ptr<Expr> self_expr;
    std::unique_ptr<Expr> other_expr;
  };

  SelfOther tuple_indexes (int idx);
  SelfOther field_acccesses (const std::string &field_name);

  /**
   * Build a suite of equality arithmetic expressions chained together by a
   * boolean AND operator
   */
  std::unique_ptr<Expr>
  build_eq_expression (std::vector<SelfOther> &&field_expressions);

  MatchCase match_enum_identifier (PathInExpression variant_path,
				   const std::unique_ptr<EnumItem> &variant);
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

#endif // ! RUST_DERIVE_PARTIAL_EQ_H
