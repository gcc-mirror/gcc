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

#ifndef RUST_AST_RESOLVE_PATTERN_H
#define RUST_AST_RESOLVE_PATTERN_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Resolver {

class ResolvePattern : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Pattern *pattern, NodeId parent)
  {
    ResolvePattern resolver (parent);
    pattern->accept_vis (resolver);
    if (resolver.resolved_node == UNKNOWN_NODEID)
      {
	rust_error_at (resolver.locus, "failed to resolve pattern %s",
		       pattern->as_string ().c_str ());
      }
  };

  void visit (AST::IdentifierPattern &pattern) override
  {
    if (resolver->get_name_scope ().lookup (
	  CanonicalPath::new_seg (pattern.get_node_id (), pattern.get_ident ()),
	  &resolved_node))
      {
	resolver->insert_resolved_name (pattern.get_node_id (), resolved_node);
	resolver->insert_new_definition (pattern.get_node_id (),
					 Definition{pattern.get_node_id (),
						    parent});
      }
  }

private:
  ResolvePattern (NodeId parent) : ResolverBase (parent) {}
};

class PatternDeclaration : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Pattern *pattern, NodeId parent)
  {
    PatternDeclaration resolver (parent);
    pattern->accept_vis (resolver);
  };

  void visit (AST::IdentifierPattern &pattern) override
  {
    // if we have a duplicate id this then allows for shadowing correctly
    // as new refs to this decl will match back here so it is ok to overwrite
    resolver->get_name_scope ().insert (
      CanonicalPath::new_seg (pattern.get_node_id (), pattern.get_ident ()),
      pattern.get_node_id (), pattern.get_locus ());
    resolver->insert_new_definition (pattern.get_node_id (),
				     Definition{pattern.get_node_id (),
						parent});
    resolver->mark_decl_mutability (pattern.get_node_id (),
				    pattern.get_is_mut ());
  }

private:
  PatternDeclaration (NodeId parent) : ResolverBase (parent) {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_PATTERN_H
