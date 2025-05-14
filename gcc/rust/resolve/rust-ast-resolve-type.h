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

#ifndef RUST_AST_RESOLVE_TYPE_H
#define RUST_AST_RESOLVE_TYPE_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-resolve-expr.h"
#include "rust-diagnostics.h"
#include "rust-hir-map.h"
#include "rust-path.h"
#include "rust-type.h"
#include "util/rust-hir-map.h"

namespace Rust {
namespace Resolver {

class ResolveRelativeTypePath
{
public:
  static bool go (AST::TypePath &path, NodeId &resolved_node_id);
};

class ResolveRelativeQualTypePath : public ResolverBase
{
  using ResolverBase::visit;

public:
  static bool go (AST::QualifiedPathInType &path);

  void visit (AST::TypePathSegmentGeneric &seg) override;

  void visit (AST::TypePathSegment &seg) override;

protected:
  bool resolve_qual_seg (AST::QualifiedPathType &seg);

private:
  ResolveRelativeQualTypePath ();

  bool failure_flag;
};

class ResolveType : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static NodeId go (AST::Type &type);

  void visit (AST::BareFunctionType &fntype) override;
  void visit (AST::TupleType &tuple) override;
  void visit (AST::TypePath &path) override;
  void visit (AST::QualifiedPathInType &path) override;
  void visit (AST::ArrayType &type) override;
  void visit (AST::ReferenceType &type) override;
  void visit (AST::InferredType &type) override;
  void visit (AST::NeverType &type) override;
  void visit (AST::RawPointerType &type) override;
  void visit (AST::TraitObjectTypeOneBound &type) override;
  void visit (AST::TraitObjectType &type) override;
  void visit (AST::ParenthesisedType &type) override;
  void visit (AST::SliceType &type) override;
  void visit (AST::ImplTraitType &type) override;
  void visit (AST::ImplTraitTypeOneBound &type) override;

private:
  ResolveType () : ResolverBase () {}
};

class ResolveTypeBound : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static NodeId go (AST::TypeParamBound &type)
  {
    ResolveTypeBound resolver;
    type.accept_vis (resolver);
    return resolver.resolved_node;
  };

  void visit (AST::TraitBound &bound) override
  {
    resolved_node = ResolveType::go (bound.get_type_path ());
  }

private:
  ResolveTypeBound () : ResolverBase () {}
};

class ResolveGenericParams : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (std::vector<std::unique_ptr<AST::GenericParam>> &params,
		  const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix)
  {
    ResolveGenericParams resolver (prefix, canonical_prefix);

    // this needs to be done in two phases as they can be used and defined later
    // in bounds
    for (auto &param : params)
      param->accept_vis (resolver);

    resolver.first_pass = false;

    for (auto &param : params)
      param->accept_vis (resolver);
  }

  static void go_single (AST::GenericParam &param, const CanonicalPath &prefix,
			 const CanonicalPath &canonical_prefix)
  {
    ResolveGenericParams resolver (prefix, canonical_prefix);

    param.accept_vis (resolver);
    resolver.first_pass = false;
    param.accept_vis (resolver);
  }

  void visit (AST::ConstGenericParam &param) override
  {
    if (first_pass)
      ResolveType::go (param.get_type ());
    else if (param.has_default_value ())
      ResolveExpr::go (param.get_default_value_unchecked ().get_expression (),
		       prefix, canonical_prefix);
  }

  void visit (AST::TypeParam &param) override
  {
    if (first_pass)
      {
	// if it has a type lets resolve it
	if (param.has_type ())
	  ResolveType::go (param.get_type ());

	auto seg = CanonicalPath::new_seg (
	  param.get_node_id (), param.get_type_representation ().as_string ());
	resolver->get_type_scope ().insert (
	  seg, param.get_node_id (), param.get_locus (), false,
	  Rib::ItemType::Type,
	  [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	    rust_error_at (param.get_locus (),
			   "generic param defined multiple times");
	    rust_error_at (locus, "was defined here");
	  });

	mappings.insert_canonical_path (param.get_node_id (), seg);
      }
    else if (param.has_type_param_bounds ())
      {
	for (auto &bound : param.get_type_param_bounds ())
	  ResolveTypeBound::go (*bound);
      }
  }

private:
  ResolveGenericParams (const CanonicalPath &prefix,
			const CanonicalPath &canonical_prefix)
    : ResolverBase (), first_pass (true), prefix (prefix),
      canonical_prefix (canonical_prefix)
  {}

  bool first_pass;
  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

class ResolveWhereClause : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void Resolve (AST::WhereClause &where_clause)
  {
    ResolveWhereClause r;
    for (auto &clause : where_clause.get_items ())
      clause->accept_vis (r);
  }

  void visit (AST::TypeBoundWhereClauseItem &item) override
  {
    ResolveType::go (item.get_type ());
    if (item.has_type_param_bounds ())
      {
	for (auto &bound : item.get_type_param_bounds ())
	  {
	    ResolveTypeBound::go (*bound);
	  }
      }
  }

private:
  ResolveWhereClause () : ResolverBase () {}
};

class ResolveTypeToCanonicalPath : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static bool go (AST::Type &type, CanonicalPath &result);

  void visit (AST::TypePath &path) override;

  void visit (AST::ReferenceType &type) override;

  void visit (AST::RawPointerType &type) override;

  void visit (AST::SliceType &type) override;

  void visit (AST::TraitObjectTypeOneBound &type) override;

  void visit (AST::TraitObjectType &type) override;

  void visit (AST::NeverType &type) override;

  void visit (AST::TupleType &type) override;

private:
  ResolveTypeToCanonicalPath ();

  CanonicalPath result;
};

class ResolveGenericArgs : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::GenericArgs &generic_args);
  static void go (AST::GenericArgs &generic_args, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix);

private:
  ResolveGenericArgs (const CanonicalPath &prefix,
		      const CanonicalPath &canonical_prefix)
    : ResolverBase (), prefix (prefix), canonical_prefix (canonical_prefix)
  {}

  bool is_type_name (const CanonicalPath &path);
  bool is_const_value_name (const CanonicalPath &path);

  /**
   * Resolve a disambiguated generic arg
   */
  void disambiguate (AST::GenericArg &arg);

  /**
   * Resolve a disambiguated generic arg
   */
  void resolve_disambiguated_generic (AST::GenericArg &arg);

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_TYPE_H
