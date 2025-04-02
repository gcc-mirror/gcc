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

#ifndef RUST_LATE_NAME_RESOLVER_2_0_H
#define RUST_LATE_NAME_RESOLVER_2_0_H

#include "rust-ast-full.h"
#include "rust-default-resolver.h"
#include "rust-expr.h"

namespace Rust {
namespace Resolver2_0 {

class Late : public DefaultResolver
{
  using DefaultResolver::visit;

public:
  Late (NameResolutionContext &ctx);

  void go (AST::Crate &crate);

  void new_label (Identifier name, NodeId id);

  // some more label declarations
  void visit (AST::LetStmt &) override;
  // TODO: Do we need this?
  // void visit (AST::Method &) override;
  void visit (AST::IdentifierPattern &) override;
  void visit (AST::SelfParam &) override;

  // resolutions
  void visit (AST::IdentifierExpr &) override;
  void visit (AST::StructExprFieldIdentifier &) override;
  void visit (AST::BreakExpr &) override;
  void visit (AST::ContinueExpr &) override;
  void visit (AST::LoopLabel &) override;
  void visit (AST::PathInExpression &) override;
  void visit (AST::TypePath &) override;
  void visit (AST::Trait &) override;
  void visit (AST::StructExprStruct &) override;
  void visit (AST::StructExprStructBase &) override;
  void visit (AST::StructExprStructFields &) override;
  void visit (AST::StructStruct &) override;
  void visit (AST::GenericArgs &) override;
  void visit (AST::GenericArg &);
  void visit (AST::ClosureExprInner &) override;
  void visit (AST::ClosureExprInnerTyped &) override;

private:
  void resolve_label (AST::Lifetime &lifetime);

  /* Setup Rust's builtin types (u8, i32, !...) in the resolver */
  void setup_builtin_types ();

  bool funny_error;
};

// TODO: Add missing mappings and data structures

} // namespace Resolver2_0
} // namespace Rust

#endif // ! RUST_LATE_NAME_RESOLVER_2_0_H
