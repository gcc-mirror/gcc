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

} // namespace AST
} // namespace Rust

#endif // ! RUST_DERIVE_CMP_COMMON_H
