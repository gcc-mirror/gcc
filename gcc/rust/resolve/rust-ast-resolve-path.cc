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

#include "rust-ast-resolve-path.h"
#include "rust-ast-resolve-type.h"
#include "rust-path.h"

namespace Rust {
namespace Resolver {

ResolvePath::ResolvePath () : ResolverBase () {}

NodeId
ResolvePath::go (AST::PathInExpression &expr)
{
  ResolvePath resolver;
  return resolver.resolve_path (expr);
}

NodeId
ResolvePath::go (AST::QualifiedPathInExpression &expr)
{
  ResolvePath resolver;
  return resolver.resolve_path (expr);
}

NodeId
ResolvePath::go (AST::SimplePath &expr)
{
  ResolvePath resolver;
  return resolver.resolve_path (expr);
}

NodeId
ResolvePath::resolve_path (AST::PathInExpression &expr)
{
  NodeId resolved_node_id = UNKNOWN_NODEID;
  NodeId module_scope_id = resolver->peek_current_module_scope ();
  NodeId previous_resolved_node_id = module_scope_id;
  for (size_t i = 0; i < expr.get_segments ().size (); i++)
    {
      auto &segment = expr.get_segments ().at (i);
      const AST::PathIdentSegment &ident_seg = segment.get_ident_segment ();
      bool is_first_segment = i == 0;
      resolved_node_id = UNKNOWN_NODEID;

      bool in_middle_of_path = i > 0;
      if (in_middle_of_path && segment.is_lower_self_seg ())
	{
	  rust_error_at (segment.get_locus (), ErrorCode::E0433,
			 "failed to resolve: %qs in paths can only be used "
			 "in start position",
			 segment.as_string ().c_str ());
	  return UNKNOWN_NODEID;
	}

      NodeId crate_scope_id = resolver->peek_crate_module_scope ();
      if (segment.is_crate_path_seg ())
	{
	  // what is the current crate scope node id?
	  module_scope_id = crate_scope_id;
	  previous_resolved_node_id = module_scope_id;
	  resolver->insert_resolved_name (segment.get_node_id (),
					  module_scope_id);
	  continue;
	}
      else if (segment.is_super_path_seg ())
	{
	  if (module_scope_id == crate_scope_id)
	    {
	      rust_error_at (segment.get_locus (),
			     "cannot use %<super%> at the crate scope");
	      return UNKNOWN_NODEID;
	    }

	  module_scope_id = resolver->peek_parent_module_scope ();
	  previous_resolved_node_id = module_scope_id;
	  resolver->insert_resolved_name (segment.get_node_id (),
					  module_scope_id);
	  continue;
	}

      // resolve any generic args
      if (segment.has_generic_args ())
	ResolveGenericArgs::go (segment.get_generic_args ());

      // logic is awkward here there are a few cases
      //
      // T::Default
      // mod::foo::impl_item
      // super::super::module::item
      // self
      // self::foo
      // self::foo::baz
      //
      // T::Default we can only resolve the T and cant do anything about Default
      // its dependant on associated types
      //
      // mod::foo::impl_item
      // we can resolve mod::foo but nothing about impl_item but we need to
      // _always resolve generic arguments
      //
      // self is a simple single lookup
      //
      // we have module_scope_id for the next module_scope to lookup
      // resolved_node_id is the thing we have resolve this segment to
      //
      // new algo?
      // we can only use module resolution when the previous segment is either
      // unknown or equal to this module_scope_id
      //
      // can only use old resolution when previous segment is unkown

      if (is_first_segment)
	{
	  // name scope first
	  NodeId resolved_node = UNKNOWN_NODEID;
	  const CanonicalPath path
	    = CanonicalPath::new_seg (segment.get_node_id (),
				      ident_seg.as_string ());
	  if (resolver->get_name_scope ().lookup (path, &resolved_node))
	    {
	      resolver->insert_resolved_name (segment.get_node_id (),
					      resolved_node);
	      resolved_node_id = resolved_node;
	    }
	  // check the type scope
	  else if (resolver->get_type_scope ().lookup (path, &resolved_node))
	    {
	      resolver->insert_resolved_type (segment.get_node_id (),
					      resolved_node);
	      resolved_node_id = resolved_node;
	    }
	  else if (segment.is_lower_self_seg ())
	    {
	      module_scope_id = crate_scope_id;
	      previous_resolved_node_id = module_scope_id;
	      resolver->insert_resolved_name (segment.get_node_id (),
					      module_scope_id);
	      continue;
	    }
	  else
	    {
	      // no error handling here since we might be able to resolve via
	      // the module hierarchy and handle errors at the end
	    }
	}

      if (resolved_node_id == UNKNOWN_NODEID
	  && previous_resolved_node_id == module_scope_id)
	{
	  tl::optional<CanonicalPath &> resolved_child
	    = mappings->lookup_module_child (module_scope_id,
					     ident_seg.as_string ());
	  if (resolved_child.has_value ())
	    {
	      NodeId resolved_node = resolved_child->get_node_id ();
	      if (resolver->get_name_scope ().decl_was_declared_here (
		    resolved_node))
		{
		  resolved_node_id = resolved_node;
		  resolver->insert_resolved_name (segment.get_node_id (),
						  resolved_node);
		}
	      else if (resolver->get_type_scope ().decl_was_declared_here (
			 resolved_node))
		{
		  resolved_node_id = resolved_node;
		  resolver->insert_resolved_type (segment.get_node_id (),
						  resolved_node);
		}
	      else
		{
		  rust_error_at (segment.get_locus (),
				 "Cannot find path %qs in this scope",
				 segment.as_string ().c_str ());
		  return UNKNOWN_NODEID;
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
	  rust_error_at (segment.get_locus (), ErrorCode::E0433,
			 "Cannot find path %qs in this scope",
			 segment.as_string ().c_str ());
	  return UNKNOWN_NODEID;
	}
    }

  resolved_node = resolved_node_id;
  if (resolved_node_id != UNKNOWN_NODEID)
    {
      // name scope first
      if (resolver->get_name_scope ().decl_was_declared_here (resolved_node_id))
	{
	  resolver->insert_resolved_name (expr.get_node_id (),
					  resolved_node_id);
	}
      // check the type scope
      else if (resolver->get_type_scope ().decl_was_declared_here (
		 resolved_node_id))
	{
	  resolver->insert_resolved_type (expr.get_node_id (),
					  resolved_node_id);
	}
      else
	{
	  rust_unreachable ();
	}
    }
  return resolved_node_id;
}

NodeId
ResolvePath::resolve_path (AST::QualifiedPathInExpression &expr)
{
  auto &root_segment = expr.get_qualified_path_type ();
  ResolveType::go (root_segment.get_type ());
  if (root_segment.has_as_clause ())
    ResolveType::go (root_segment.get_as_type_path ());

  for (auto &segment : expr.get_segments ())
    {
      // we cant actually do anything with the segment itself since this is all
      // the job of the type system to figure it out but we can resolve any
      // generic arguments used
      if (segment.has_generic_args ())
	ResolveGenericArgs::go (segment.get_generic_args ());
    }

  // cannot fully resolve a qualified path as it is dependant on associated
  // items
  return UNKNOWN_NODEID;
}

NodeId
ResolvePath::resolve_path (AST::SimplePath &expr)
{
  NodeId crate_scope_id = resolver->peek_crate_module_scope ();
  NodeId module_scope_id = resolver->peek_current_module_scope ();

  NodeId previous_resolved_node_id = UNKNOWN_NODEID;
  NodeId resolved_node_id = UNKNOWN_NODEID;
  for (size_t i = 0; i < expr.get_segments ().size (); i++)
    {
      AST::SimplePathSegment &segment = expr.get_segments ().at (i);
      bool is_first_segment = i == 0;
      bool is_final_segment = i >= (expr.get_segments ().size () - 1);
      resolved_node_id = UNKNOWN_NODEID;

      if (segment.is_crate_path_seg ())
	{
	  // what is the current crate scope node id?
	  module_scope_id = crate_scope_id;
	  previous_resolved_node_id = module_scope_id;
	  resolver->insert_resolved_name (segment.get_node_id (),
					  module_scope_id);
	  resolved_node_id = module_scope_id;

	  continue;
	}
      else if (segment.is_super_path_seg ())
	{
	  if (module_scope_id == crate_scope_id)
	    {
	      rust_error_at (segment.get_locus (),
			     "cannot use %<super%> at the crate scope");
	      return UNKNOWN_NODEID;
	    }

	  module_scope_id = resolver->peek_parent_module_scope ();
	  previous_resolved_node_id = module_scope_id;
	  resolver->insert_resolved_name (segment.get_node_id (),
					  module_scope_id);
	  resolved_node_id = module_scope_id;

	  continue;
	}

      tl::optional<CanonicalPath &> resolved_child
	= mappings->lookup_module_child (module_scope_id,
					 segment.get_segment_name ());
      if (resolved_child.has_value ())
	{
	  NodeId resolved_node = resolved_child->get_node_id ();
	  if (resolver->get_name_scope ().decl_was_declared_here (
		resolved_node))
	    {
	      resolved_node_id = resolved_node;
	      resolver->insert_resolved_name (segment.get_node_id (),
					      resolved_node);
	    }
	  else if (resolver->get_type_scope ().decl_was_declared_here (
		     resolved_node))
	    {
	      resolved_node_id = resolved_node;
	      resolver->insert_resolved_type (segment.get_node_id (),
					      resolved_node);
	    }
	  else
	    {
	      rust_error_at (segment.get_locus (),
			     "Cannot find path %qs in this scope",
			     segment.as_string ().c_str ());
	      return UNKNOWN_NODEID;
	    }
	}

      if (resolved_node_id == UNKNOWN_NODEID && is_first_segment)
	{
	  // name scope first
	  NodeId resolved_node = UNKNOWN_NODEID;
	  const CanonicalPath path
	    = CanonicalPath::new_seg (segment.get_node_id (),
				      segment.get_segment_name ());
	  if (resolver->get_name_scope ().lookup (path, &resolved_node))
	    {
	      resolved_node_id = resolved_node;
	      resolver->insert_resolved_name (segment.get_node_id (),
					      resolved_node);
	    }
	  // check the type scope
	  else if (resolver->get_type_scope ().lookup (path, &resolved_node))
	    {
	      resolved_node_id = resolved_node;
	      resolver->insert_resolved_type (segment.get_node_id (),
					      resolved_node);
	    }
	}

      // if we still have not resolved and this is the final segment and the
      // final segment is self its likely the case: pub use
      //
      // result::Result::{self, Err, Ok};
      //
      // Then the resolved_node_id is just the previous one so long as it is a
      // resolved node id
      // rust_debug_loc (segment.get_locus (),
      //   	      "trying to resolve seg: [%s] first [%s] last [%s]",
      //   	      segment.get_segment_name ().c_str (),
      //   	      is_first_segment ? "true" : "false",
      //   	      is_final_segment ? "true" : "false");
      if (resolved_node_id == UNKNOWN_NODEID && !is_first_segment
	  && is_final_segment && segment.is_lower_self_seg ())
	resolved_node_id = previous_resolved_node_id;

      // final check
      if (resolved_node_id == UNKNOWN_NODEID)
	{
	  rust_error_at (segment.get_locus (),
			 "cannot find simple path segment %qs in this scope",
			 segment.as_string ().c_str ());
	  return UNKNOWN_NODEID;
	}

      if (mappings->node_is_module (resolved_node_id))
	{
	  module_scope_id = resolved_node_id;
	}

      previous_resolved_node_id = resolved_node_id;
    }

  resolved_node = resolved_node_id;
  if (resolved_node_id != UNKNOWN_NODEID)
    {
      // name scope first
      if (resolver->get_name_scope ().decl_was_declared_here (resolved_node_id))
	{
	  resolver->insert_resolved_name (expr.get_node_id (),
					  resolved_node_id);
	}
      // check the type scope
      else if (resolver->get_type_scope ().decl_was_declared_here (
		 resolved_node_id))
	{
	  resolver->insert_resolved_type (expr.get_node_id (),
					  resolved_node_id);
	}
      else
	{
	  rust_unreachable ();
	}
    }
  return resolved_node_id;
}

} // namespace Resolver
} // namespace Rust
