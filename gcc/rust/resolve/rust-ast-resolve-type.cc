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

#include "rust-ast-resolve-type.h"
#include "rust-ast-resolve-expr.h"

namespace Rust {
namespace Resolver {

// rust-ast-resolve-type.h

std::string
ResolveTypeToCanonicalPath::canonicalize_generic_args (AST::GenericArgs &args)
{
  std::string buf;

  size_t i = 0;
  size_t total = args.get_type_args ().size ();

  for (auto &ty_arg : args.get_type_args ())
    {
      buf += ty_arg->as_string ();
      if ((i + 1) < total)
	buf += ",";

      i++;
    }

  return "<" + buf + ">";
}

bool
ResolveTypeToCanonicalPath::type_resolve_generic_args (AST::GenericArgs &args)
{
  for (auto &gt : args.get_type_args ())
    {
      ResolveType::go (gt.get (), UNKNOWN_NODEID);
      // FIXME error handling here for inference variable since they do not have
      // a node to resolve to
      // if (resolved == UNKNOWN_NODEID) return false;
    }
  return true;
}

void
ResolveTypeToCanonicalPath::visit (AST::TypePathSegmentGeneric &seg)
{
  if (seg.is_error ())
    {
      failure_flag = true;
      rust_error_at (seg.get_locus (), "segment has error: %s",
		     seg.as_string ().c_str ());
      return;
    }

  if (!seg.has_generic_args ())
    {
      auto ident_segment
	= CanonicalPath::new_seg (seg.get_node_id (),
				  seg.get_ident_segment ().as_string ());
      result = result.append (ident_segment);
      return;
    }

  if (type_resolve_generic_args_flag)
    {
      bool ok = type_resolve_generic_args (seg.get_generic_args ());
      failure_flag = !ok;
    }

  if (include_generic_args_flag)
    {
      std::string generics
	= canonicalize_generic_args (seg.get_generic_args ());
      auto generic_segment
	= CanonicalPath::new_seg (seg.get_node_id (),
				  seg.get_ident_segment ().as_string ()
				    + "::" + generics);
      result = result.append (generic_segment);
      return;
    }

  auto ident_segment
    = CanonicalPath::new_seg (seg.get_node_id (),
			      seg.get_ident_segment ().as_string ());
  result = result.append (ident_segment);
}

void
ResolveTypeToCanonicalPath::visit (AST::TypePathSegment &seg)
{
  if (seg.is_error ())
    {
      failure_flag = true;
      rust_error_at (seg.get_locus (), "segment has error: %s",
		     seg.as_string ().c_str ());
      return;
    }

  CanonicalPath ident_seg
    = CanonicalPath::new_seg (seg.get_node_id (),
			      seg.get_ident_segment ().as_string ());
  result = result.append (ident_seg);
}

void
ResolveType::visit (AST::ArrayType &type)
{
  type.get_elem_type ()->accept_vis (*this);
  // FIXME
  // the capacity expr can contain block-expr with functions but these should be
  // folded via constexpr code
  ResolveExpr::go (type.get_size_expr ().get (), type.get_node_id (),
		   CanonicalPath::create_empty (),
		   CanonicalPath::create_empty ());
}

void
ResolveType::visit (AST::TraitObjectTypeOneBound &type)
{
  NodeId bound_resolved_id
    = ResolveTypeBound::go (&type.get_trait_bound (), type.get_node_id ());
  ok = bound_resolved_id != UNKNOWN_NODEID;
}

void
ResolveType::visit (AST::TraitObjectType &type)
{
  ok = true;
  for (auto &bound : type.get_type_param_bounds ())
    {
      /* NodeId bound_resolved_id = */
      ResolveTypeBound::go (bound.get (), type.get_node_id ());
    }
}

void
ResolveTypeToCanonicalPath::visit (AST::ReferenceType &ref)
{
  auto inner_type
    = ResolveTypeToCanonicalPath::resolve (*ref.get_type_referenced ().get (),
					   include_generic_args_flag,
					   type_resolve_generic_args_flag);

  std::string segment_string ("&");
  if (ref.get_has_mut ())
    segment_string += "mut ";

  segment_string += inner_type.get ();

  auto ident_seg = CanonicalPath::new_seg (ref.get_node_id (), segment_string);
  result = result.append (ident_seg);
}

void
ResolveTypeToCanonicalPath::visit (AST::RawPointerType &ref)
{
  auto inner_type
    = ResolveTypeToCanonicalPath::resolve (*ref.get_type_pointed_to ().get (),
					   include_generic_args_flag,
					   type_resolve_generic_args_flag);

  std::string segment_string ("*");
  switch (ref.get_pointer_type ())
    {
    case AST::RawPointerType::PointerType::MUT:
      segment_string += "mut ";
      break;

    case AST::RawPointerType::PointerType::CONST:
      segment_string += "const ";
      break;
    }

  segment_string += inner_type.get ();

  auto ident_seg = CanonicalPath::new_seg (ref.get_node_id (), segment_string);
  result = result.append (ident_seg);
}

void
ResolveTypeToCanonicalPath::visit (AST::SliceType &slice)
{
  auto inner_type
    = ResolveTypeToCanonicalPath::resolve (*slice.get_elem_type ().get (),
					   include_generic_args_flag,
					   type_resolve_generic_args_flag);
  std::string segment_string = "[" + inner_type.get () + "]";
  auto ident_seg
    = CanonicalPath::new_seg (slice.get_node_id (), segment_string);
  result = result.append (ident_seg);
}

void
ResolveType::visit (AST::ReferenceType &type)
{
  type.get_type_referenced ()->accept_vis (*this);

  if (canonical_path != nullptr && canonical_path->size () > 0)
    {
      std::string seg = canonical_path->get ();
      *canonical_path = CanonicalPath::new_seg (type.get_node_id (), "&" + seg);
    }
}

void
ResolveType::visit (AST::RawPointerType &type)
{
  type.get_type_pointed_to ()->accept_vis (*this);

  if (canonical_path != nullptr && canonical_path->size () > 0)
    {
      std::string seg = canonical_path->get ();
      *canonical_path = CanonicalPath::new_seg (type.get_node_id (), "*" + seg);
    }
}

void
ResolveType::visit (AST::InferredType &type)
{
  ok = true;
}

void
ResolveType::visit (AST::NeverType &type)
{
  ok = true;
}

void
ResolveType::visit (AST::SliceType &type)
{
  type.get_elem_type ()->accept_vis (*this);
}

ResolveRelativeTypePath::ResolveRelativeTypePath (CanonicalPath qualified_path)
  : ResolveTypeToCanonicalPath (true, true)
{
  result = qualified_path;
}

bool
ResolveRelativeTypePath::go (AST::QualifiedPathInType &path)
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
} // namespace Resolver
} // namespace Rust
