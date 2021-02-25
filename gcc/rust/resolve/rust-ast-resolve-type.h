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

class ResolveTypePath : public ResolverBase
{
public:
  static NodeId go (AST::TypePath &path, NodeId parent)
  {
    ResolveTypePath resolver (parent);
    resolver.resolve (path);
    return resolver.resolved_node;
  }

  void visit (AST::TypePathSegmentGeneric &seg) override;

  void visit (AST::TypePathSegment &seg) override;

private:
  void resolve (AST::TypePath &path)
  {
    for (auto &seg : path.get_segments ())
      {
	seg->accept_vis (*this);
	if (type_seg_failed_flag)
	  return;
      }

    if (path_buffer.empty ())
      {
	rust_error_at (path.get_locus (), "failed to resolve path: %s",
		       path.as_string ().c_str ());
	return;
      }

    if (!resolver->get_type_scope ().lookup (path_buffer, &resolved_node))
      {
	rust_error_at (path.get_locus (), "failed to resolve TypePath: %s",
		       path_buffer.c_str ());
	return;
      }
  }

  ResolveTypePath (NodeId parent)
    : ResolverBase (parent), type_seg_failed_flag (false)
  {}

  std::string path_buffer;
  bool type_seg_failed_flag;
};

class ResolveType : public ResolverBase
{
public:
  static NodeId go (AST::Type *type, NodeId parent)
  {
    ResolveType resolver (parent);
    type->accept_vis (resolver);
    if (!resolver.ok)
      rust_error_at (type->get_locus_slow (), "unresolved type");

    return resolver.resolved_node;
  };

  void visit (AST::BareFunctionType &fntype)
  {
    ok = true;
    for (auto &param : fntype.get_function_params ())
      ResolveType::go (param.get_type ().get (), fntype.get_node_id ());

    if (fntype.has_return_type ())
      ResolveType::go (fntype.get_return_type ().get (), fntype.get_node_id ());
  }

  void visit (AST::TupleType &tuple)
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

  void visit (AST::TypePath &path)
  {
    resolved_node = ResolveTypePath::go (path, parent);
    ok = resolved_node != UNKNOWN_NODEID;
    if (ok)
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

  void visit (AST::ReferenceType &type)
  {
    type.get_type_referenced ()->accept_vis (*this);
  }

  // nothing to do for inferred types
  void visit (AST::InferredType &type) { ok = true; }

private:
  ResolveType (NodeId parent) : ResolverBase (parent), ok (false) {}

  bool ok;
};

class ResolveGenericParam : public ResolverBase
{
public:
  static NodeId go (AST::GenericParam *param, NodeId parent)
  {
    ResolveGenericParam resolver (parent);
    param->accept_vis (resolver);
    if (!resolver.ok)
      rust_error_at (param->get_locus_slow (), "unresolved generic parameter");

    return resolver.resolved_node;
  };

  void visit (AST::TypeParam &param) override
  {
    ok = true;

    // for now lets focus on handling the basics: like struct<T> { a:T, ....}
    resolver->get_type_scope ().insert (
      param.get_type_representation (), param.get_node_id (),
      param.get_locus (), false,
      [&] (std::string, NodeId, Location locus) -> void {
	rust_error_at (param.get_locus (),
		       "generic param redefined multiple times");
	rust_error_at (locus, "was defined here");
      });
  }

private:
  ResolveGenericParam (NodeId parent) : ResolverBase (parent), ok (false) {}

  bool ok;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_TYPE_H
