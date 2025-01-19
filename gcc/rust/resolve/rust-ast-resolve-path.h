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

#ifndef RUST_AST_RESOLVE_PATH_H
#define RUST_AST_RESOLVE_PATH_H

#include "rust-ast-resolve-base.h"

namespace Rust {
namespace Resolver {

class ResolvePath : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static NodeId go (AST::PathInExpression &expr);
  static NodeId go (AST::QualifiedPathInExpression &expr);
  static NodeId go (AST::SimplePath &expr);

private:
  ResolvePath ();

  NodeId resolve_path (AST::PathInExpression &expr);
  NodeId resolve_path (AST::QualifiedPathInExpression &expr);
  NodeId resolve_path (AST::SimplePath &expr);

  void
  resolve_simple_path_segments (CanonicalPath prefix, size_t offs,
				const std::vector<AST::SimplePathSegment> &segs,
				NodeId expr_node_id, location_t expr_locus);
};

} // namespace Resolver
} // namespace Rust

#endif // !RUST_AST_RESOLVE_PATH_H
