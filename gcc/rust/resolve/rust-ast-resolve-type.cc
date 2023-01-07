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

#include "rust-ast-resolve-type.h"
#include "rust-ast-resolve-expr.h"

namespace Rust {
namespace Resolver {

// rust-ast-resolve-type.h

void
ResolveType::visit (AST::ArrayType &type)
{
  type.get_elem_type ()->accept_vis (*this);
  ResolveExpr::go (type.get_size_expr ().get (), CanonicalPath::create_empty (),
		   CanonicalPath::create_empty ());
}

void
ResolveType::visit (AST::TraitObjectTypeOneBound &type)
{
  ResolveTypeBound::go (&type.get_trait_bound ());
}

void
ResolveType::visit (AST::TraitObjectType &type)
{
  for (auto &bound : type.get_type_param_bounds ())
    {
      /* NodeId bound_resolved_id = */
      ResolveTypeBound::go (bound.get ());
    }
}

void
ResolveType::visit (AST::ReferenceType &type)
{
  resolved_node = ResolveType::go (type.get_type_referenced ().get ());
}

void
ResolveType::visit (AST::RawPointerType &type)
{
  resolved_node = ResolveType::go (type.get_type_pointed_to ().get ());
}

void
ResolveType::visit (AST::InferredType &)
{
  // FIXME
}

void
ResolveType::visit (AST::NeverType &)
{
  // FIXME
}

void
ResolveType::visit (AST::SliceType &type)
{
  resolved_node = ResolveType::go (type.get_elem_type ().get ());
}

// resolve relative type-paths

bool
ResolveRelativeTypePath::go (AST::TypePath &path, NodeId &resolved_node_id)
{
  auto resolver = Resolver::get ();
  auto mappings = Analysis::Mappings::get ();

  NodeId module_scope_id = resolver->peek_current_module_scope ();
  NodeId previous_resolved_node_id = module_scope_id;
  for (size_t i = 0; i < path.get_segments ().size (); i++)
    {
      auto &segment = path.get_segments ().at (i);
      const AST::PathIdentSegment &ident_seg = segment->get_ident_segment ();
      bool is_first_segment = i == 0;
      resolved_node_id = UNKNOWN_NODEID;

      bool in_middle_of_path = i > 0;
      if (in_middle_of_path && segment->is_lower_self_seg ())
	{
	  // error[E0433]: failed to resolve: `self` in paths can only be used
	  // in start position
	  rust_error_at (segment->get_locus (),
			 "failed to resolve: %<%s%> in paths can only be used "
			 "in start position",
			 segment->as_string ().c_str ());
	  return false;
	}

      NodeId crate_scope_id = resolver->peek_crate_module_scope ();
      if (segment->is_crate_path_seg ())
	{
	  // what is the current crate scope node id?
	  module_scope_id = crate_scope_id;
	  previous_resolved_node_id = module_scope_id;
	  resolver->insert_resolved_name (segment->get_node_id (),
					  module_scope_id);

	  continue;
	}
      else if (segment->is_super_path_seg ())
	{
	  if (module_scope_id == crate_scope_id)
	    {
	      rust_error_at (segment->get_locus (),
			     "cannot use super at the crate scope");
	      return false;
	    }

	  module_scope_id = resolver->peek_parent_module_scope ();
	  previous_resolved_node_id = module_scope_id;
	  resolver->insert_resolved_name (segment->get_node_id (),
					  module_scope_id);
	  continue;
	}

      switch (segment->get_type ())
	{
	  case AST::TypePathSegment::SegmentType::GENERIC: {
	    AST::TypePathSegmentGeneric *s
	      = static_cast<AST::TypePathSegmentGeneric *> (segment.get ());
	    if (s->has_generic_args ())
	      ResolveGenericArgs::go (s->get_generic_args ());
	  }
	  break;

	case AST::TypePathSegment::SegmentType::REG:
	  // nothing to do
	  break;

	case AST::TypePathSegment::SegmentType::FUNCTION:
	  AST::TypePathSegmentFunction *fnseg
	    = static_cast<AST::TypePathSegmentFunction *> (segment.get ());

	  AST::TypePathFunction &fn = fnseg->get_type_path_function ();
	  for (auto &param : fn.get_params ())
	    {
	      ResolveType::go (param.get ());
	    }

	  if (fn.has_return_type ())
	    {
	      ResolveType::go (fn.get_return_type ().get ());
	    }

	  break;
	}

      if (is_first_segment)
	{
	  // name scope first
	  NodeId resolved_node = UNKNOWN_NODEID;
	  const CanonicalPath path
	    = CanonicalPath::new_seg (segment->get_node_id (),
				      ident_seg.as_string ());
	  if (resolver->get_type_scope ().lookup (path, &resolved_node))
	    {
	      resolver->insert_resolved_type (segment->get_node_id (),
					      resolved_node);
	      resolved_node_id = resolved_node;
	    }
	  else if (resolver->get_name_scope ().lookup (path, &resolved_node))
	    {
	      resolver->insert_resolved_name (segment->get_node_id (),
					      resolved_node);
	      resolved_node_id = resolved_node;
	    }
	  else if (segment->is_lower_self_seg ())
	    {
	      // what is the current crate scope node id?
	      module_scope_id = crate_scope_id;
	      previous_resolved_node_id = module_scope_id;
	      resolver->insert_resolved_name (segment->get_node_id (),
					      module_scope_id);

	      continue;
	    }
	}

      if (resolved_node_id == UNKNOWN_NODEID
	  && previous_resolved_node_id == module_scope_id)
	{
	  Optional<CanonicalPath &> resolved_child
	    = mappings->lookup_module_child (module_scope_id,
					     ident_seg.as_string ());
	  if (resolved_child.is_some ())
	    {
	      NodeId resolved_node = resolved_child->get_node_id ();
	      if (resolver->get_name_scope ().decl_was_declared_here (
		    resolved_node))
		{
		  resolved_node_id = resolved_node;
		  resolver->insert_resolved_name (segment->get_node_id (),
						  resolved_node);
		}
	      else if (resolver->get_type_scope ().decl_was_declared_here (
			 resolved_node))
		{
		  resolved_node_id = resolved_node;
		  resolver->insert_resolved_type (segment->get_node_id (),
						  resolved_node);
		}
	      else
		{
		  rust_error_at (segment->get_locus (),
				 "Cannot find path %<%s%> in this scope",
				 segment->as_string ().c_str ());
		  return false;
		}
	    }
	}

      bool did_resolve_segment = resolved_node_id != UNKNOWN_NODEID;
      if (did_resolve_segment)
	{
	  if (mappings->node_is_module (resolved_node_id)
	      || mappings->node_is_crate (resolved_node_id))
	    {
	      module_scope_id = resolved_node_id;
	    }
	  previous_resolved_node_id = resolved_node_id;
	}
      else if (is_first_segment)
	{
	  rust_error_at (segment->get_locus (),
			 "failed to resolve TypePath: %s in this scope",
			 segment->as_string ().c_str ());
	  return false;
	}
    }

  if (resolved_node_id != UNKNOWN_NODEID)
    {
      // name scope first
      if (resolver->get_name_scope ().decl_was_declared_here (resolved_node_id))
	{
	  resolver->insert_resolved_name (path.get_node_id (),
					  resolved_node_id);
	}
      // check the type scope
      else if (resolver->get_type_scope ().decl_was_declared_here (
		 resolved_node_id))
	{
	  resolver->insert_resolved_type (path.get_node_id (),
					  resolved_node_id);
	}
      else
	{
	  gcc_unreachable ();
	}
    }

  return true;
}

// qualified type paths

ResolveRelativeQualTypePath::ResolveRelativeQualTypePath ()
  : failure_flag (false)
{}

bool
ResolveRelativeQualTypePath::go (AST::QualifiedPathInType &path)
{
  ResolveRelativeQualTypePath o;

  // resolve the type and trait path
  auto &qualified_path = path.get_qualified_path_type ();
  if (!o.resolve_qual_seg (qualified_path))
    return false;

  // qualified types are similar to other paths in that we cannot guarantee
  // that we can resolve the path at name resolution. We must look up
  // associated types and type information to figure this out properly

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

bool
ResolveRelativeQualTypePath::resolve_qual_seg (AST::QualifiedPathType &seg)
{
  if (seg.is_error ())
    {
      rust_error_at (seg.get_locus (), "segment has error: %s",
		     seg.as_string ().c_str ());
      return false;
    }

  auto type = seg.get_type ().get ();
  NodeId type_resolved_node = ResolveType::go (type);
  if (type_resolved_node == UNKNOWN_NODEID)
    return false;

  if (!seg.has_as_clause ())
    return true;

  NodeId trait_resolved_node = ResolveType::go (&seg.get_as_type_path ());
  if (trait_resolved_node == UNKNOWN_NODEID)
    return false;

  return true;
}

void
ResolveRelativeQualTypePath::visit (AST::TypePathSegmentGeneric &seg)
{
  if (seg.is_error ())
    {
      failure_flag = true;
      rust_error_at (seg.get_locus (), "segment has error: %s",
		     seg.as_string ().c_str ());
      return;
    }

  ResolveGenericArgs::go (seg.get_generic_args ());
}

void
ResolveRelativeQualTypePath::visit (AST::TypePathSegment &seg)
{
  if (seg.is_error ())
    {
      failure_flag = true;
      rust_error_at (seg.get_locus (), "segment has error: %s",
		     seg.as_string ().c_str ());
      return;
    }
}

// resolve to canonical path

bool
ResolveTypeToCanonicalPath::go (AST::Type *type, CanonicalPath &result)
{
  ResolveTypeToCanonicalPath resolver;
  type->accept_vis (resolver);
  result = resolver.result;
  return !resolver.result.is_empty ();
}

void
ResolveTypeToCanonicalPath::visit (AST::TypePath &path)
{
  NodeId resolved_node = UNKNOWN_NODEID;
  if (!resolver->lookup_resolved_name (path.get_node_id (), &resolved_node))
    {
      resolver->lookup_resolved_type (path.get_node_id (), &resolved_node);
    }

  if (resolved_node == UNKNOWN_NODEID)
    return;

  const CanonicalPath *type_path = nullptr;
  if (mappings->lookup_canonical_path (resolved_node, &type_path))
    {
      auto &final_seg = path.get_segments ().back ();
      switch (final_seg->get_type ())
	{
	  case AST::TypePathSegment::SegmentType::GENERIC: {
	    AST::TypePathSegmentGeneric *s
	      = static_cast<AST::TypePathSegmentGeneric *> (final_seg.get ());

	    std::vector<CanonicalPath> args;
	    if (s->has_generic_args ())
	      {
		ResolveGenericArgs::go (s->get_generic_args ());
		for (auto &generic : s->get_generic_args ().get_generic_args ())
		  {
		    // FIXME: What do we want to do here in case there is a
		    // constant or an ambiguous const generic?
		    // TODO: At that point, will all generics have been
		    // disambiguated? Can we thus canonical resolve types and
		    // const and `gcc_unreachable` on ambiguous types?
		    // This is probably fine as we just want to canonicalize
		    // types, right?
		    if (generic.get_kind () == AST::GenericArg::Kind::Type)
		      {
			CanonicalPath arg = CanonicalPath::create_empty ();
			bool ok = ResolveTypeToCanonicalPath::go (
			  generic.get_type ().get (), arg);
			if (ok)
			  args.push_back (std::move (arg));
		      }
		  }
	      }

	    result = *type_path;
	    if (!args.empty ())
	      {
		// append this onto the path
		std::string buf;
		for (size_t i = 0; i < args.size (); i++)
		  {
		    bool has_next = (i + 1) < args.size ();
		    const auto &arg = args.at (i);

		    buf += arg.get ();
		    if (has_next)
		      buf += ", ";
		  }

		std::string arg_seg = "<" + buf + ">";
		CanonicalPath argument_seg
		  = CanonicalPath::new_seg (s->get_node_id (), arg_seg);
		result = result.append (argument_seg);
	      }
	  }
	  break;

	default:
	  result = *type_path;
	  break;
	}
    }
}

void
ResolveTypeToCanonicalPath::visit (AST::ReferenceType &type)
{
  CanonicalPath path = CanonicalPath::create_empty ();
  bool ok
    = ResolveTypeToCanonicalPath::go (type.get_type_referenced ().get (), path);
  if (ok)
    {
      std::string ref_type_str = type.is_mut () ? "mut" : "";
      std::string ref_path = "&" + ref_type_str + " " + path.get ();
      result = CanonicalPath::new_seg (type.get_node_id (), ref_path);
    }
}

void
ResolveTypeToCanonicalPath::visit (AST::RawPointerType &type)
{
  CanonicalPath path = CanonicalPath::create_empty ();
  bool ok
    = ResolveTypeToCanonicalPath::go (type.get_type_pointed_to ().get (), path);
  if (ok)
    {
      std::string ptr_type_str
	= type.get_pointer_type () == AST::RawPointerType::CONST ? "const"
								 : "mut";
      std::string ptr_path = "*" + ptr_type_str + " " + path.get ();
      result = CanonicalPath::new_seg (type.get_node_id (), ptr_path);
    }
}

void
ResolveTypeToCanonicalPath::visit (AST::SliceType &type)
{
  CanonicalPath path = CanonicalPath::create_empty ();
  bool ok = ResolveTypeToCanonicalPath::go (type.get_elem_type ().get (), path);
  if (ok)
    {
      std::string slice_path = "[" + path.get () + "]";
      result = CanonicalPath::new_seg (type.get_node_id (), slice_path);
    }
}

void
ResolveTypeToCanonicalPath::visit (AST::TraitObjectTypeOneBound &type)
{
  CanonicalPath path = CanonicalPath::create_empty ();
  bool ok
    = ResolveTypeToCanonicalPath::go (&type.get_trait_bound ().get_type_path (),
				      path);
  if (ok)
    {
      std::string slice_path = "<dyn " + path.get () + ">";
      result = CanonicalPath::new_seg (type.get_node_id (), slice_path);
    }
}

void
ResolveTypeToCanonicalPath::visit (AST::TraitObjectType &)
{
  // FIXME is this actually allowed? dyn A+B
  gcc_unreachable ();
}

ResolveTypeToCanonicalPath::ResolveTypeToCanonicalPath ()
  : ResolverBase (), result (CanonicalPath::create_empty ())
{}

bool
ResolveGenericArgs::is_const_value_name (const CanonicalPath &path)
{
  NodeId resolved;
  auto found = resolver->get_name_scope ().lookup (path, &resolved);

  return found;
}

bool
ResolveGenericArgs::is_type_name (const CanonicalPath &path)
{
  NodeId resolved;
  auto found = resolver->get_type_scope ().lookup (path, &resolved);

  return found;
}

void
ResolveGenericArgs::disambiguate (AST::GenericArg &arg)
{
  auto path = canonical_prefix.append (
    CanonicalPath::new_seg (UNKNOWN_NODEID, arg.get_path ()));

  auto is_type = is_type_name (path);
  auto is_value = is_const_value_name (path);

  // In case we cannot find anything, we resolve the ambiguity to a type.
  // This causes the typechecker to error out properly and when necessary.
  // But types also take priority over const values in the case of
  // ambiguities, hence the weird control flow
  if (is_type || (!is_type && !is_value))
    arg = arg.disambiguate_to_type ();
  else if (is_value)
    arg = arg.disambiguate_to_const ();
}

void
ResolveGenericArgs::resolve_disambiguated_generic (AST::GenericArg &arg)
{
  switch (arg.get_kind ())
    {
    case AST::GenericArg::Kind::Const:
      ResolveExpr::go (arg.get_expression ().get (), prefix, canonical_prefix);
      break;
    case AST::GenericArg::Kind::Type:
      ResolveType::go (arg.get_type ().get ());
      break;
    default:
      gcc_unreachable ();
    }
}
void
ResolveGenericArgs::go (AST::GenericArgs &generic_args)
{
  auto empty = CanonicalPath::create_empty ();

  go (generic_args, empty, empty);
}

void
ResolveGenericArgs::go (AST::GenericArgs &generic_args,
			const CanonicalPath &prefix,
			const CanonicalPath &canonical_prefix)
{
  auto resolver = ResolveGenericArgs (prefix, canonical_prefix);

  for (auto &arg : generic_args.get_generic_args ())
    {
      if (arg.get_kind () == AST::GenericArg::Kind::Either)
	resolver.disambiguate (arg);

      resolver.resolve_disambiguated_generic (arg);
    }

  for (auto &binding : generic_args.get_binding_args ())
    {
      ResolveType::go (binding.get_type ().get ());
    }
}

} // namespace Resolver
} // namespace Rust
