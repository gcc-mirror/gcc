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

#ifndef RUST_COMPILE_RESOLVE_PATH
#define RUST_COMPILE_RESOLVE_PATH

#include "rust-compile-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Compile {

class ResolvePathRef : public HIRCompileBase, public HIR::HIRPatternVisitor
{
public:
  static tree Compile (HIR::QualifiedPathInExpression &expr, Context *ctx)
  {
    ResolvePathRef resolver (ctx);
    expr.accept_vis (resolver);
    return resolver.resolved;
  }

  static tree Compile (HIR::PathInExpression &expr, Context *ctx)
  {
    ResolvePathRef resolver (ctx);
    expr.accept_vis (resolver);
    return resolver.resolved;
  }

  void visit (HIR::PathInExpression &expr) override;
  void visit (HIR::QualifiedPathInExpression &expr) override;

  // Empty visit for unused Pattern HIR nodes.
  void visit (HIR::IdentifierPattern &) override {}
  void visit (HIR::LiteralPattern &) override {}
  void visit (HIR::RangePattern &) override {}
  void visit (HIR::ReferencePattern &) override {}
  void visit (HIR::SlicePattern &) override {}
  void visit (HIR::AltPattern &) override {}
  void visit (HIR::StructPattern &) override {}
  void visit (HIR::TuplePattern &) override {}
  void visit (HIR::TupleStructPattern &) override {}
  void visit (HIR::WildcardPattern &) override {}

  ResolvePathRef (Context *ctx)
    : HIRCompileBase (ctx), resolved (error_mark_node)
  {}

  tree resolve (const HIR::PathIdentSegment &final_segment,
		const Analysis::NodeMapping &mappings, location_t locus,
		bool is_qualified_path);

  tree resolved;

private:
  tree
  attempt_constructor_expression_lookup (TyTy::BaseType *lookup, Context *ctx,
					 const Analysis::NodeMapping &mappings,
					 location_t expr_locus);
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_RESOLVE_PATH
