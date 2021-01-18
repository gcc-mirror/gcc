// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_AST_RESOLVE_STRUCT_EXPR_FIELD
#define RUST_AST_RESOLVE_STRUCT_EXPR_FIELD

#include "rust-ast-resolve-base.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Resolver {

// this resolves values being assigned not that the field actually exists yet.
// We cant resolve the field to struct until type resolution since the HIR
// Mappings don't exist yet.
class ResolveStructExprField : public ResolverBase
{
public:
  static void go (AST::StructExprField *field, NodeId parent)
  {
    ResolveStructExprField resolver (parent);
    field->accept_vis (resolver);
  }

  virtual ~ResolveStructExprField () {}

  void visit (AST::StructExprFieldIdentifierValue &field);

  void visit (AST::StructExprFieldIndexValue &field);

private:
  ResolveStructExprField (NodeId parent) : ResolverBase (parent) {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_STRUCT_EXPR_FIELD
