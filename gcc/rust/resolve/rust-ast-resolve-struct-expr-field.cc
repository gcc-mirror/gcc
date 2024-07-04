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

#include "rust-ast-resolve-struct-expr-field.h"
#include "rust-ast-resolve-expr.h"

namespace Rust {
namespace Resolver {

void
ResolveStructExprField::go (AST::StructExprField *field,
			    const CanonicalPath &prefix,
			    const CanonicalPath &canonical_prefix)
{
  ResolveStructExprField resolver (prefix, canonical_prefix);
  field->accept_vis (resolver);
}

ResolveStructExprField::ResolveStructExprField (
  const CanonicalPath &prefix, const CanonicalPath &canonical_prefix)
  : ResolverBase (), prefix (prefix), canonical_prefix (canonical_prefix)
{}

void
ResolveStructExprField::visit (AST::StructExprFieldIdentifierValue &field)
{
  ResolveExpr::go (field.get_value ().get (), prefix, canonical_prefix);
}

void
ResolveStructExprField::visit (AST::StructExprFieldIndexValue &field)
{
  ResolveExpr::go (field.get_value ().get (), prefix, canonical_prefix);
}

void
ResolveStructExprField::visit (AST::StructExprFieldIdentifier &field)
{
  AST::IdentifierExpr expr (field.get_field_name (), {}, field.get_locus ());
  expr.set_node_id (field.get_node_id ());

  ResolveExpr::go (&expr, prefix, canonical_prefix);
}

} // namespace Resolver
} // namespace Rust
