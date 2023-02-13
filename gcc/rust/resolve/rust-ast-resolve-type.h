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

#ifndef RUST_AST_RESOLVE_TYPE_H
#define RUST_AST_RESOLVE_TYPE_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-resolve-expr.h"
#include "rust-ast-full.h"

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
  static NodeId go (AST::Type *type)
  {
    ResolveType resolver;
    type->accept_vis (resolver);
    return resolver.resolved_node;
  }

  void visit (AST::BareFunctionType &fntype) override
  {
    for (auto &param : fntype.get_function_params ())
      ResolveType::go (param.get_type ().get ());

    if (fntype.has_return_type ())
      ResolveType::go (fntype.get_return_type ().get ());
  }

  void visit (AST::TupleType &tuple) override
  {
    if (tuple.is_unit_type ())
      {
	resolved_node = resolver->get_unit_type_node_id ();
	return;
      }

    for (auto &elem : tuple.get_elems ())
      ResolveType::go (elem.get ());
  }

  void visit (AST::TypePath &path) override
  {
    ResolveRelativeTypePath::go (path, resolved_node);
  }

  void visit (AST::QualifiedPathInType &path) override
  {
    ResolveRelativeQualTypePath::go (path);
  }

  void visit (AST::ArrayType &type) override;

  void visit (AST::ReferenceType &type) override;

  void visit (AST::InferredType &type) override;

  void visit (AST::NeverType &type) override;

  void visit (AST::RawPointerType &type) override;

  void visit (AST::TraitObjectTypeOneBound &type) override;

  void visit (AST::TraitObjectType &type) override;

  void visit (AST::SliceType &type) override;

private:
  ResolveType () : ResolverBase () {}
};

class ResolveTypeBound : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static NodeId go (AST::TypeParamBound *type)
  {
    ResolveTypeBound resolver;
    type->accept_vis (resolver);
    return resolver.resolved_node;
  };

  void visit (AST::TraitBound &bound) override
  {
    resolved_node = ResolveType::go (&bound.get_type_path ());
  }

private:
  ResolveTypeBound () : ResolverBase () {}
};

class ResolveGenericParam : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static NodeId go (AST::GenericParam *param, const CanonicalPath &prefix,
		    const CanonicalPath &canonical_prefix)
  {
    ResolveGenericParam resolver (prefix, canonical_prefix);
    param->accept_vis (resolver);
    return resolver.resolved_node;
  }

  void visit (AST::ConstGenericParam &param) override
  {
    ResolveType::go (param.get_type ().get ());

    if (param.has_default_value ())
      ResolveExpr::go (param.get_default_value ().get_expression ().get (),
		       prefix, canonical_prefix);

    ok = true;
  }

  void visit (AST::TypeParam &param) override
  {
    // if it has a type lets resolve it
    if (param.has_type ())
      ResolveType::go (param.get_type ().get ());

    if (param.has_type_param_bounds ())
      {
	for (auto &bound : param.get_type_param_bounds ())
	  {
	    ResolveTypeBound::go (bound.get ());
	  }
      }

    auto seg = CanonicalPath::new_seg (param.get_node_id (),
				       param.get_type_representation ());
    resolver->get_type_scope ().insert (
      seg, param.get_node_id (), param.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	rust_error_at (param.get_locus (),
		       "generic param redefined multiple times");
	rust_error_at (locus, "was defined here");
      });

    mappings->insert_canonical_path (param.get_node_id (), seg);
  }

private:
  ResolveGenericParam (const CanonicalPath &prefix,
		       const CanonicalPath &canonical_prefix)
    : ResolverBase (), ok (false), prefix (prefix),
      canonical_prefix (canonical_prefix)
  {}

  bool ok;
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
    ResolveType::go (item.get_type ().get ());
    if (item.has_type_param_bounds ())
      {
	for (auto &bound : item.get_type_param_bounds ())
	  {
	    ResolveTypeBound::go (bound.get ());
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
  static bool go (AST::Type *type, CanonicalPath &result);

  void visit (AST::TypePath &path) override;

  void visit (AST::ReferenceType &type) override;

  void visit (AST::RawPointerType &type) override;

  void visit (AST::SliceType &type) override;

  void visit (AST::TraitObjectTypeOneBound &type) override;

  void visit (AST::TraitObjectType &type) override;

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
