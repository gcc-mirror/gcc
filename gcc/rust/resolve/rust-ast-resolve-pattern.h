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

// Specifies whether the set of already bound patterns are related by 'Or' or
// 'Product'. Used to check for multiple bindings to the same identifier.
enum PatternBoundCtx
{
  // A product pattern context (e.g. struct and tuple patterns)
  Product,
  // An or-pattern context (e.g. p_0 | p_1 | ...)
  Or,
};

struct PatternBinding
{
  PatternBoundCtx ctx;
  std::set<Identifier> idents;

  PatternBinding (PatternBoundCtx ctx, std::set<Identifier> idents)
    : ctx (ctx), idents (idents)
  {}
};

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
  static void go (AST::Pattern *pattern, Rib::ItemType type);
  static void go (AST::Pattern *pattern, Rib::ItemType type,
		  std::vector<PatternBinding> &bindings);

  void visit (AST::IdentifierPattern &pattern) override;
  void visit (AST::GroupedPattern &pattern) override;
  void visit (AST::ReferencePattern &pattern) override;
  void visit (AST::PathInExpression &pattern) override;
  void visit (AST::StructPattern &pattern) override;
  void visit (AST::TupleStructPattern &pattern) override;
  void visit (AST::TuplePattern &pattern) override;
  void visit (AST::RangePattern &pattern) override;

private:
  PatternDeclaration (std::vector<PatternBinding> &bindings, Rib::ItemType type)
    : ResolverBase (), bindings (bindings), type (type)
  {}

  std::vector<PatternBinding> &bindings;
  Rib::ItemType type;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_PATTERN_H
