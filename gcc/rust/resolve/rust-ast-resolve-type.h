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

#ifndef RUST_AST_RESOLVE_TYPE_H
#define RUST_AST_RESOLVE_TYPE_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Resolver {

class ResolveType : public ResolverBase
{
public:
  static void go (AST::Type *type, NodeId parent)
  {
    ResolveType resolver (parent);

    type->accept_vis (resolver);
    if (resolver.resolved_node == UNKNOWN_NODEID)
      {
	rust_error_at (resolver.locus, "failed to resolve type %s",
		       type->as_string ().c_str ());
      }
  };

  ~ResolveType () {}

  void visit (AST::TypePath &path)
  {
    // this will need changed to handle mod/crate/use globs and look
    // at the segments in granularity
    locus = path.get_locus ();
    if (resolver->get_type_scope ().lookup (path.as_string (), &resolved_node))
      {
	resolver->insert_resolved_type (path.get_node_id (), resolved_node);
	resolver->insert_new_definition (path.get_node_id (),
					 Definition{path.get_node_id (),
						    parent});
      }
  }

  void visit (AST::ArrayType &type)
  {
    type.get_elem_type ()->accept_vis (*this);
  }

private:
  ResolveType (NodeId parent) : ResolverBase (parent) {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_TYPE_H
