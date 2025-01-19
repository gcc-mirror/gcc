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

// Info that gets stored in the map. Helps us detect if two bindings to the same
// identifier have different mutability or ref states.
class BindingTypeInfo
{
  Mutability mut;
  bool is_ref;
  location_t locus;

public:
  BindingTypeInfo (Mutability mut, bool is_ref, location_t locus)
    : mut (mut), is_ref (is_ref), locus (locus)
  {}

  BindingTypeInfo (BindingTypeInfo const &other)
    : mut (other.mut), is_ref (other.is_ref), locus (other.get_locus ())
  {}

  BindingTypeInfo (){};

  location_t get_locus () const { return locus; }
  Mutability get_mut () const { return mut; }
  bool get_is_ref () const { return is_ref; }

  BindingTypeInfo operator= (BindingTypeInfo const &other)
  {
    mut = other.mut;
    is_ref = other.is_ref;
    locus = other.get_locus ();

    return *this;
  }

  bool operator== (BindingTypeInfo const &other)
  {
    return mut == other.mut && is_ref == other.is_ref;
  }

  bool operator!= (BindingTypeInfo const &other)
  {
    return !BindingTypeInfo::operator== (other);
  }
};

typedef std::map<Identifier, BindingTypeInfo> BindingMap;

class PatternDeclaration : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Pattern &pattern, Rib::ItemType type);
  static void go (AST::Pattern &pattern, Rib::ItemType type,
		  std::vector<PatternBinding> &bindings);

  void visit (AST::IdentifierPattern &pattern) override;
  void visit (AST::GroupedPattern &pattern) override;
  void visit (AST::ReferencePattern &pattern) override;
  void visit (AST::PathInExpression &pattern) override;
  void visit (AST::StructPattern &pattern) override;
  void visit (AST::TupleStructPattern &pattern) override;
  void visit (AST::TuplePattern &pattern) override;
  void visit (AST::RangePattern &pattern) override;
  void visit (AST::AltPattern &pattern) override;
  void visit (AST::SlicePattern &pattern) override;

  void add_new_binding (Identifier ident, NodeId node_id, BindingTypeInfo info);
  void check_bindings_consistency (std::vector<BindingMap> &binding_maps);

private:
  PatternDeclaration (std::vector<PatternBinding> &bindings_with_ctx,
		      Rib::ItemType type)
    : ResolverBase (), bindings_with_ctx (bindings_with_ctx), type (type)
  {}

  // To avoid having a separate visitor for consistency checks, we store
  // bindings in two forms:

  // 1) Bindings as a vector of context-related sets.
  // Used for checking multiple bindings to the same identifier (i.e. E0415,
  // E0416).
  std::vector<PatternBinding> &bindings_with_ctx;

  // 2) Bindings as a map between identifiers and binding info.
  // Used for checking consistency between alt patterns (i.e. E0408, E0409).
  BindingMap binding_info_map;

  // we need to insert the missing and inconsistent bindings (found in
  // check_bindings_consistency) into maps to avoid duplication of error
  // messages.
  BindingMap inconsistent_bindings;
  BindingMap missing_bindings;

  Rib::ItemType type;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_PATTERN_H
