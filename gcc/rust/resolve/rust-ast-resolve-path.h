// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#ifndef RUST_AST_RESOLVE_PATH_H
#define RUST_AST_RESOLVE_PATH_H

#include "rust-ast-resolve-base.h"

namespace Rust {
namespace Resolver {

class ResolvePath : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::PathInExpression *expr, NodeId parent);
  static void go (AST::QualifiedPathInExpression *expr, NodeId parent);
  static void go (AST::SimplePath *expr, NodeId parent);

private:
  ResolvePath (NodeId parent) : ResolverBase (parent) {}

  void resolve_path (AST::PathInExpression *expr);
  void resolve_path (AST::QualifiedPathInExpression *expr);
  void resolve_path (AST::SimplePath *expr);

  void resolve_segments (CanonicalPath prefix, size_t offs,
			 std::vector<AST::PathExprSegment> &segs,
			 NodeId expr_node_id, Location expr_locus);

  void
  resolve_simple_path_segments (CanonicalPath prefix, size_t offs,
				const std::vector<AST::SimplePathSegment> &segs,
				NodeId expr_node_id, Location expr_locus);
};

class ResolveSimplePathSegmentToCanonicalPath
{
public:
  static CanonicalPath resolve (const AST::SimplePathSegment &seg)
  {
    // FIXME: Since this is so simple, maybe it can simply be a tiny function?
    return CanonicalPath::new_seg (seg.get_node_id (), seg.get_segment_name ());
  }
};

} // namespace Resolver
} // namespace Rust

#endif // !RUST_AST_RESOLVE_PATH_H
