// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

namespace Rust {
namespace Resolver {

// this resolves values being assigned not that the field actually exists yet.

class ResolveStructExprField : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::StructExprField *field, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix);

  void visit (AST::StructExprFieldIdentifierValue &field) override;

  void visit (AST::StructExprFieldIndexValue &field) override;

  void visit (AST::StructExprFieldIdentifier &field) override;

private:
  ResolveStructExprField (const CanonicalPath &prefix,
			  const CanonicalPath &canonical_prefix);

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_STRUCT_EXPR_FIELD
