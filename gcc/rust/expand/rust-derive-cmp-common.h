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

#ifndef RUST_DERIVE_CMP_COMMON_H
#define RUST_DERIVE_CMP_COMMON_H

#include "rust-ast.h"
#include "rust-ast-builder.h"
#include "rust-item.h"
#include "rust-path.h"

namespace Rust {
namespace AST {

/**
 * A pair of two expressions from each instance being compared. E.g. this
 * could be `self.0` and `other.0`, or `self.field` and `other.field`
 */
struct SelfOther
{
  std::unique_ptr<Expr> self_expr;
  std::unique_ptr<Expr> other_expr;

  /* Create a <self.i> and an <other.i> expression */
  static SelfOther index (Builder builder, int idx);
  static std::vector<SelfOther> indexes (Builder builder,
					 const std::vector<TupleField> &fields);

  /* Create a <self.field> and an <other.field> expression */
  static SelfOther field (Builder builder, const std::string &field_name);
  static std::vector<SelfOther> fields (Builder builder,
					const std::vector<StructField> &fields);
};

/**
 * Builder for common match cases used when comparing two enum instances. This
 * builder takes care of creating the unique patterns for the `self` instance
 * and `other` instance, as well as the entire `MatchCase` required for building
 * a proper comparision expression for an implementation of a comparision trait
 * for an enum type. The functions take a lambda to use when creating the
 * expression of the generated `MatchCase`.
 */
class EnumMatchBuilder
{
public:
  /**
   * The type of functions to call when creating the resulting expression in the
   * generated `MatchCase`
   */
  using ExprFn
    = std::function<std::unique_ptr<Expr> (std::vector<SelfOther> &&)>;

  EnumMatchBuilder (const std::string &enum_path,
		    const std::string &variant_path, ExprFn fn,
		    Builder &builder)
    : enum_path (enum_path), variant_path (variant_path), fn (fn),
      builder (builder)
  {}

  /**
   * Generate a `MatchCase` for an enum tuple variant
   *
   * (&Enum::Tuple(self0, self1), &Enum::Tuple(other0, other1)) => <fn>
   */
  MatchCase tuple (EnumItem &variant);

  /**
   * Generate a `MatchCase` for an enum struct variant
   *
   * (&Enum::Struct { a: self_a }, &Enum::Struct { a: other_a }) => <fn>
   */
  MatchCase strukt (EnumItem &variant);

private:
  const std::string &enum_path;
  const std::string &variant_path;
  ExprFn fn;
  Builder &builder;
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_DERIVE_CMP_COMMON_H
