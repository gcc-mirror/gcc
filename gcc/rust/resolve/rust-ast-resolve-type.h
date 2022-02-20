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

#ifndef RUST_AST_RESOLVE_TYPE_H
#define RUST_AST_RESOLVE_TYPE_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Resolver {

class ResolveConstantItemToCanonicalPath
{
public:
  static CanonicalPath resolve (AST::ConstantItem &constant)
  {
    return CanonicalPath::new_seg (constant.get_node_id (),
				   constant.get_identifier ());
  }
};

class ResolveFunctionItemToCanonicalPath
{
public:
  static CanonicalPath resolve (AST::Function &function)
  {
    return CanonicalPath::new_seg (function.get_node_id (),
				   function.get_function_name ());
  }
};

class ResolveMethodItemToCanonicalPath
{
public:
  static CanonicalPath resolve (AST::Method &method)
  {
    return CanonicalPath::new_seg (method.get_node_id (),
				   method.get_method_name ());
  }
};

class ResolveTraitItemFunctionToCanonicalPath
{
public:
  static CanonicalPath resolve (AST::TraitItemFunc &function)
  {
    return CanonicalPath::new_seg (
      function.get_node_id (),
      function.get_trait_function_decl ().get_identifier ());
  }
};

class ResolveTraitItemMethodToCanonicalPath
{
public:
  static CanonicalPath resolve (AST::TraitItemMethod &method)
  {
    return CanonicalPath::new_seg (
      method.get_node_id (), method.get_trait_method_decl ().get_identifier ());
  }
};

class ResolveTraitItemConstToCanonicalPath
{
public:
  static CanonicalPath resolve (AST::TraitItemConst &constant)
  {
    return CanonicalPath::new_seg (constant.get_node_id (),
				   constant.get_identifier ());
  }
};

class ResolveTraitItemTypeToCanonicalPath
{
public:
  static CanonicalPath resolve (AST::TraitItemType &type)
  {
    return CanonicalPath::new_seg (type.get_node_id (), type.get_identifier ());
  }
};

class ResolveTypeToCanonicalPath : public ResolverBase
{
protected:
  using Rust::Resolver::ResolverBase::visit;

public:
  // FIXME this should really only take AST::TypeNoBounds&
  static CanonicalPath resolve (AST::Type &type,
				bool include_generic_args = true,
				bool type_resolve_generic_args = true)
  {
    ResolveTypeToCanonicalPath resolver (include_generic_args,
					 type_resolve_generic_args);
    type.accept_vis (resolver);
    return resolver.result;
  }

  void visit (AST::TypePath &path) override
  {
    for (auto &seg : path.get_segments ())
      {
	seg->accept_vis (*this);
	if (failure_flag)
	  return;
      }
  }

  void visit (AST::ReferenceType &ref) override;

  void visit (AST::TypePathSegmentGeneric &seg) override;

  void visit (AST::TypePathSegment &seg) override;

  static std::string canonicalize_generic_args (AST::GenericArgs &args);

  static bool type_resolve_generic_args (AST::GenericArgs &args);

protected:
  ResolveTypeToCanonicalPath (bool include_generic_args,
			      bool type_resolve_generic_args)
    : ResolverBase (UNKNOWN_NODEID), result (CanonicalPath::create_empty ()),
      include_generic_args_flag (include_generic_args),
      type_resolve_generic_args_flag (type_resolve_generic_args),
      failure_flag (false)
  {}

  CanonicalPath result;
  bool include_generic_args_flag;
  bool type_resolve_generic_args_flag;
  bool failure_flag;
};

class ResolvePathSegmentToCanonicalPath
{
public:
  static CanonicalPath resolve (AST::PathExprSegment &seg)
  {
    if (!seg.has_generic_args ())
      return CanonicalPath::new_seg (seg.get_node_id (),
				     seg.get_ident_segment ().as_string ());

    bool ok = ResolveTypeToCanonicalPath::type_resolve_generic_args (
      seg.get_generic_args ());
    if (!ok)
      {
	rust_error_at (seg.get_locus (),
		       "failed to resolve all generic arguments");
	return CanonicalPath::create_empty ();
      }

    std::string generics
      = ResolveTypeToCanonicalPath::canonicalize_generic_args (
	seg.get_generic_args ());

    return CanonicalPath::new_seg (seg.get_node_id (),
				   seg.get_ident_segment ().as_string ()
				     + "::" + generics);
  }
};

class TraitImplProjection
{
public:
  static CanonicalPath resolve (NodeId id, const CanonicalPath &trait_seg,
				const CanonicalPath &impl_type_seg)
  {
    return CanonicalPath::new_seg (id, "<" + impl_type_seg.get () + " as "
					 + trait_seg.get () + ">");
  }
};

class ResolveRelativeTypePath : public ResolveTypeToCanonicalPath
{
  using ResolveTypeToCanonicalPath::visit;

public:
  static bool go (AST::QualifiedPathInType &path, NodeId parent,
		  bool canonicalize_type_with_generics)
  {
    // resolve the type and trait path
    auto &qualified_path = path.get_qualified_path_type ();
    CanonicalPath result = CanonicalPath::create_empty ();
    if (!resolve_qual_seg (qualified_path, result))
      return false;

    // resolve the associated impl if available but it can also be from a trait
    // and this is allowed to fail
    auto resolver = Resolver::get ();
    NodeId projection_resolved_id = UNKNOWN_NODEID;
    if (resolver->get_name_scope ().lookup (result, &projection_resolved_id))
      {
	// mark the resolution for this
	resolver->insert_resolved_name (qualified_path.get_node_id (),
					projection_resolved_id);
      }

    // qualified types are similar to other paths in that we cannot guarantee
    // that we can resolve the path at name resolution. We must look up
    // associated types and type information to figure this out properly

    ResolveRelativeTypePath o (result);
    std::unique_ptr<AST::TypePathSegment> &associated
      = path.get_associated_segment ();

    associated->accept_vis (o);
    if (o.failure_flag)
      return false;

    for (auto &seg : path.get_segments ())
      {
	seg->accept_vis (o);
	if (o.failure_flag)
	  return false;
      }

    return true;
  }

private:
  ResolveRelativeTypePath (CanonicalPath qualified_path)
    : ResolveTypeToCanonicalPath (true, true)
  {
    result = qualified_path;
  }

  static bool resolve_qual_seg (AST::QualifiedPathType &seg,
				CanonicalPath &result);
};

class ResolveType : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static NodeId go (AST::Type *type, NodeId parent,
		    bool canonicalize_type_with_generics = false,
		    CanonicalPath *canonical_path = nullptr)
  {
    ResolveType resolver (parent, canonicalize_type_with_generics,
			  canonical_path);
    type->accept_vis (resolver);
    if (!resolver.ok)
      rust_error_at (type->get_locus (), "unresolved type");

    return resolver.resolved_node;
  };

  void visit (AST::BareFunctionType &fntype) override
  {
    ok = true;
    for (auto &param : fntype.get_function_params ())
      ResolveType::go (param.get_type ().get (), fntype.get_node_id ());

    if (fntype.has_return_type ())
      ResolveType::go (fntype.get_return_type ().get (), fntype.get_node_id ());
  }

  void visit (AST::TupleType &tuple) override
  {
    ok = true;
    if (tuple.is_unit_type ())
      {
	resolved_node = resolver->get_unit_type_node_id ();
	return;
      }

    for (auto &elem : tuple.get_elems ())
      ResolveType::go (elem.get (), tuple.get_node_id ());
  }

  void visit (AST::TypePath &path) override
  {
    auto rel_canonical_path
      = ResolveTypeToCanonicalPath::resolve (path,
					     canonicalize_type_with_generics,
					     true);
    if (rel_canonical_path.is_empty ())
      {
	rust_error_at (path.get_locus (),
		       "Failed to resolve canonical path for TypePath");
	return;
      }

    ok = !rel_canonical_path.is_empty ();

    // lets try and resolve in one go else leave it up to the type resolver to
    // figure outer

    if (resolver->get_type_scope ().lookup (rel_canonical_path, &resolved_node))
      {
	resolver->insert_resolved_type (path.get_node_id (), resolved_node);
	resolver->insert_new_definition (path.get_node_id (),
					 Definition{path.get_node_id (),
						    parent});

	if (canonical_path != nullptr)
	  {
	    const CanonicalPath *cpath = nullptr;
	    bool ok
	      = mappings->lookup_canonical_path (mappings->get_current_crate (),
						 resolved_node, &cpath);
	    if (!ok)
	      {
		*canonical_path = rel_canonical_path;
	      }
	    else
	      {
		*canonical_path = *cpath;
	      }
	  }

	return;
      }

    // lets resolve as many segments as we can and leave it up to the type
    // resolver otherwise
    size_t nprocessed = 0;
    rel_canonical_path.iterate ([&] (const CanonicalPath &seg) -> bool {
      resolved_node = UNKNOWN_NODEID;

      if (!resolver->get_type_scope ().lookup (seg, &resolved_node))
	return false;

      resolver->insert_resolved_type (seg.get_node_id (), resolved_node);
      resolver->insert_new_definition (seg.get_node_id (),
				       Definition{path.get_node_id (), parent});
      nprocessed++;
      return true;
    });

    if (nprocessed == 0)
      {
	rust_error_at (path.get_locus (), "failed to resolve TypePath: %s",
		       path.as_string ().c_str ());
	return;
      }

    // its ok if this fails since the type resolver sometimes will need to
    // investigate the bounds of a type for the associated type for example see:
    // https://github.com/Rust-GCC/gccrs/issues/746
    if (nprocessed == rel_canonical_path.size ())
      {
	resolver->insert_resolved_type (path.get_node_id (), resolved_node);
	resolver->insert_new_definition (path.get_node_id (),
					 Definition{path.get_node_id (),
						    parent});

	if (canonical_path != nullptr)
	  {
	    const CanonicalPath *cpath = nullptr;
	    bool ok
	      = mappings->lookup_canonical_path (mappings->get_current_crate (),
						 resolved_node, &cpath);
	    rust_assert (ok);
	    *canonical_path = *cpath;
	  }
      }
  }

  void visit (AST::QualifiedPathInType &path) override
  {
    ok = ResolveRelativeTypePath::go (path, parent,
				      canonicalize_type_with_generics);
  }

  void visit (AST::ArrayType &type) override;

  void visit (AST::ReferenceType &type) override;

  void visit (AST::InferredType &type) override;

  void visit (AST::RawPointerType &type) override;

  void visit (AST::TraitObjectTypeOneBound &type) override;

  void visit (AST::TraitObjectType &type) override;

  void visit (AST::SliceType &type) override;

private:
  ResolveType (NodeId parent, bool canonicalize_type_with_generics,
	       CanonicalPath *canonical_path)
    : ResolverBase (parent),
      canonicalize_type_with_generics (canonicalize_type_with_generics),
      ok (false), canonical_path (canonical_path)
  {}

  bool canonicalize_type_with_generics;
  bool ok;
  CanonicalPath *canonical_path;
};

class ResolveTypeBound : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static NodeId go (AST::TypeParamBound *type, NodeId parent,
		    bool canonicalize_type_with_generics = false)
  {
    ResolveTypeBound resolver (parent, canonicalize_type_with_generics);
    type->accept_vis (resolver);
    if (!resolver.ok)
      rust_error_at (type->get_locus (), "unresolved type bound");

    return resolver.resolved_node;
  };

  void visit (AST::TraitBound &bound) override
  {
    resolved_node = ResolveType::go (&bound.get_type_path (), parent,
				     canonicalize_type_with_generics);
    ok = resolved_node != UNKNOWN_NODEID;
  }

  void visit (AST::Lifetime &bound) override { ok = true; }

private:
  ResolveTypeBound (NodeId parent, bool canonicalize_type_with_generics)
    : ResolverBase (parent),
      canonicalize_type_with_generics (canonicalize_type_with_generics),
      ok (false)
  {}

  bool canonicalize_type_with_generics;
  bool ok;
};

class ResolveGenericParam : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static NodeId go (AST::GenericParam *param, NodeId parent)
  {
    ResolveGenericParam resolver (parent);
    param->accept_vis (resolver);
    if (!resolver.ok)
      rust_error_at (param->get_locus (), "unresolved generic parameter");

    return resolver.resolved_node;
  };

  void visit (AST::LifetimeParam &param) override
  {
    // For now do not do anything and accept everything.
    ok = true;
  }

  void visit (AST::TypeParam &param) override
  {
    ok = true;

    // if it has a type lets resolve it
    if (param.has_type ())
      ResolveType::go (param.get_type ().get (), param.get_node_id ());

    if (param.has_type_param_bounds ())
      {
	for (auto &bound : param.get_type_param_bounds ())
	  {
	    ResolveTypeBound::go (bound.get (), param.get_node_id ());
	  }
      }

    auto seg = CanonicalPath::new_seg (param.get_node_id (),
				       param.get_type_representation ());
    resolver->get_type_scope ().insert (
      seg, param.get_node_id (), param.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	rust_error_at (param.get_locus (),
		       "generic param redefined multiple times");
	rust_error_at (locus, "was defined here");
      });

    mappings->insert_canonical_path (mappings->get_current_crate (),
				     param.get_node_id (), seg);
  }

private:
  ResolveGenericParam (NodeId parent) : ResolverBase (parent), ok (false) {}

  bool ok;
};

class ResolveWhereClause : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void Resolve (AST::WhereClause &where_clause)
  {
    ResolveWhereClause r (where_clause.get_node_id ());
    for (auto &clause : where_clause.get_items ())
      clause->accept_vis (r);
  }

  void visit (AST::LifetimeWhereClauseItem &) override
  {
    // nothing to do
  }

  void visit (AST::TypeBoundWhereClauseItem &item) override
  {
    ResolveType::go (item.get_type ().get (), item.get_node_id ());
    if (item.has_type_param_bounds ())
      {
	for (auto &bound : item.get_type_param_bounds ())
	  {
	    ResolveTypeBound::go (bound.get (), item.get_node_id ());
	  }
      }
  }

private:
  ResolveWhereClause (NodeId parent) : ResolverBase (parent) {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_TYPE_H
