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

namespace Rust {
namespace Compile {

class ResolvePathRef : public HIRCompileBase
{
public:
  static tree Compile (HIR::QualifiedPathInExpression &expr, Context *ctx);

  static tree Compile (HIR::PathInExpression &expr, Context *ctx);

  ResolvePathRef (Context *ctx);

  /**
   * Generic visitor for both PathInExpression and QualifiedPathInExpression
   */
  template <typename T> tree resolve_path_like (T &expr);

  /**
   * Inner implementation of `resolve` - resolution with an already known NodeId
   */
  tree resolve_with_node_id (const HIR::PathIdentSegment &final_segment,
			     const Analysis::NodeMapping &mappings,
			     location_t locus, bool is_qualified_path,
			     NodeId resolved_node_id);
  /**
   * Resolve a mappings' NodeId and call into `resolve_with_node_id` which
   * performs the rest of the path resolution
   */
  tree resolve (const HIR::PathIdentSegment &final_segment,
		const Analysis::NodeMapping &mappings, location_t locus,
		bool is_qualified_path);

private:
  tree
  attempt_constructor_expression_lookup (TyTy::BaseType *lookup, Context *ctx,
					 const Analysis::NodeMapping &mappings,
					 location_t expr_locus);
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_RESOLVE_PATH
