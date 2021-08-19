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

#ifndef RUST_COMPILE_RESOLVE_PATH
#define RUST_COMPILE_RESOLVE_PATH

#include "rust-compile-base.h"
#include "rust-compile-tyty.h"

namespace Rust {
namespace Compile {

class ResolvePathRef : public HIRCompileBase
{
  using Rust::Compile::HIRCompileBase::visit;

public:
  static Bexpression *Compile (HIR::QualifiedPathInExpression &expr,
			       Context *ctx)
  {
    ResolvePathRef resolver (ctx);
    expr.accept_vis (resolver);
    return resolver.resolved;
  }

  static Bexpression *Compile (HIR::PathInExpression &expr, Context *ctx)
  {
    ResolvePathRef resolver (ctx);
    expr.accept_vis (resolver);
    return resolver.resolved;
  }

  void visit (HIR::PathInExpression &expr) override;

  void visit (HIR::QualifiedPathInExpression &expr) override;

private:
  ResolvePathRef (Context *ctx)
    : HIRCompileBase (ctx), resolved (ctx->get_backend ()->error_expression ())
  {}

  void resolve (const HIR::PathIdentSegment &final_segment,
		const Analysis::NodeMapping &mappings, Location locus,
		bool is_qualified_path);

  Bexpression *resolved;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_RESOLVE_PATH
