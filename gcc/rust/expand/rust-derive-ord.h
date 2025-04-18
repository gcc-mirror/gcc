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

#ifndef RUST_DERIVE_ORD_H
#define RUST_DERIVE_ORD_H

#include "rust-ast.h"
#include "rust-derive-cmp-common.h"
#include "rust-derive.h"

namespace Rust {
namespace AST {

/**
 * DeriveOrd is a bit special as the expansion of both `PartialOrd` and `Ord`
 * is extremely similar. The only difference is that `PartialOrd` concerns
 * partial-ordering, and thus its main method returns an `Option<Ordering>`,
 * while `Ord` concerns total-ordering, and its main method returns an
 * `Ordering`. Otherwise, the expansion logic is the same, so we factor both
 * derives into one.
 */
class DeriveOrd : public DeriveVisitor
{
public:
  enum class Ordering
  {
    Total,
    Partial
  };

  std::string fn (Ordering ordering)
  {
    if (ordering == Ordering::Total)
      return "cmp";
    else
      return "partial_cmp";
  }

  std::string trait (Ordering ordering)
  {
    if (ordering == Ordering::Total)
      return "Ord";
    else
      return "PartialOrd";
  }

  DeriveOrd (Ordering ordering, location_t loc);

  std::unique_ptr<Item> go (Item &item);

private:
  std::unique_ptr<Item> expanded;

  Ordering ordering;

  /* Identifier patterns for the non-equal match arms */
  constexpr static const char *not_equal = "non_eq";

  /**
   * Create the recursive matching structure used when implementing the
   * comparison function on multiple sub items (fields, tuple indexes...)
   */
  std::unique_ptr<Expr> recursive_match (std::vector<SelfOther> &&members);

  /**
   * Make the match arms for one inner match in a comparison function block.
   * This returns the "equal" match arm and the "rest" match arm, so something
   * like `Ordering::Equal` and `non_eq` in the following match expression:
   *
   * match cmp(...) {
   *     Ordering::Equal => match cmp(...) { ... }
   *     non_eq => non_eq,
   * }
   */
  std::pair<MatchArm, MatchArm> make_cmp_arms ();

  std::unique_ptr<Item>
  cmp_impl (std::unique_ptr<BlockExpr> &&fn_block, Identifier type_name,
	    const std::vector<std::unique_ptr<GenericParam>> &type_generics);
  std::unique_ptr<AssociatedItem> cmp_fn (std::unique_ptr<BlockExpr> &&block,
					  Identifier type_name);

  virtual void visit_struct (StructStruct &item) override;
  virtual void visit_tuple (TupleStruct &item) override;
  virtual void visit_enum (Enum &item) override;
  virtual void visit_union (Union &item) override;
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_DERIVE_ORD_H
