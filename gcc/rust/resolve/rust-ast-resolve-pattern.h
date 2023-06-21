// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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
  static void go (AST::Pattern *pattern)
  {
    ResolvePattern resolver;
    pattern->accept_vis (resolver);
  }

  void visit (AST::IdentifierPattern &pattern) override
  {
    if (resolver->get_name_scope ().lookup (
	  CanonicalPath::new_seg (pattern.get_node_id (), pattern.get_ident ()),
	  &resolved_node))
      {
	resolver->insert_resolved_name (pattern.get_node_id (), resolved_node);
      }
  }

private:
  ResolvePattern () : ResolverBase () {}
};

class PatternDeclaration : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Pattern *pattern, Rib::ItemType type)
  {
    PatternDeclaration resolver (type);
    pattern->accept_vis (resolver);
  };

  void visit (AST::IdentifierPattern &pattern) override
  {
    // if we have a duplicate id this then allows for shadowing correctly
    // as new refs to this decl will match back here so it is ok to overwrite
    resolver->get_name_scope ().insert (
      CanonicalPath::new_seg (pattern.get_node_id (), pattern.get_ident ()),
      pattern.get_node_id (), pattern.get_locus (), type);
  }

  void visit (AST::GroupedPattern &pattern) override
  {
    pattern.get_pattern_in_parens ()->accept_vis (*this);
  }

  // cases in a match expression
  void visit (AST::PathInExpression &pattern) override;

  void visit (AST::StructPattern &pattern) override;

  void visit (AST::TupleStructPattern &pattern) override;

  void visit (AST::TuplePattern &pattern) override;

  void visit (AST::RangePattern &pattern) override;

private:
  PatternDeclaration (Rib::ItemType type) : ResolverBase (), type (type) {}

  Rib::ItemType type;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_PATTERN_H
