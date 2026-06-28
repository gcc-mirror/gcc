// Copyright (C) 2026 Free Software Foundation, Inc.

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

#include "optional.h"
#include "rust-forever-stack.h"
#include "rust-name-resolution-context.h"

/**
 * Split the actual path resolution logic in its own file because it's a lot,
 * and it could get even worse for certain edge cases.
 */

namespace Rust {
namespace Resolver2_0 {

template <Namespace N>
bool
NameResolutionContext::should_search_prelude (
  const typename ForeverStack<N>::Node *current_node,
  const typename ForeverStack<N>::SegIterator &iterator,
  const std::vector<ResolutionPath::Segment> &segments)
{
  // Check whether the current_node is a root node
  if (current_node->is_root ())
    return true;

  // Check whether we're at the start of a module (we can't travel elsewhere
  // from the start of a module)
  if (is_start (iterator, segments)
      && current_node->rib (N).kind == Rib::Kind::Module)
    return true;

  return false;
}

template <Namespace N>
tl::optional<Rib::Definition>
NameResolutionContext::resolve_path (
  ForeverStack<N> &stack, const ResolutionPath &path, ResolutionMode mode,
  std::function<void (Usage, Definition, Namespace)> insert_segment_resolution,
  std::vector<Error> &collect_errors)
{
  std::reference_wrapper<typename ForeverStack<N>::Node> starting_point
    = stack.cursor ();

  return NameResolutionContext::resolve_path (stack, path, mode,
					      insert_segment_resolution,
					      collect_errors, starting_point);
}

template <Namespace N>
tl::optional<Rib::Definition>
NameResolutionContext::resolve_path (
  ForeverStack<N> &stack, const ResolutionPath &path, ResolutionMode mode,
  std::function<void (Usage, Definition, Namespace)> insert_segment_resolution,
  std::vector<Error> &collect_errors, NodeId starting_point_id)

{
  auto starting_point = stack.dfs_node (stack.root, starting_point_id);

  // We may have a prelude, but haven't visited it yet and thus it's not in
  // our nodes
  if (!starting_point)
    return tl::nullopt;

  return NameResolutionContext::resolve_path (stack, path, mode,
					      insert_segment_resolution,
					      collect_errors, *starting_point);
}

template <Namespace N>
tl::optional<Rib::Definition>
NameResolutionContext::resolve_path (
  ForeverStack<N> &stack, const ResolutionPath &path, ResolutionMode mode,
  std::function<void (Usage, Definition, Namespace)> insert_segment_resolution,
  std::vector<Error> &collect_errors,
  std::reference_wrapper<typename ForeverStack<N>::Node> starting_point)
{
  bool can_descend = true;

  rust_debug ("resolving %s", path.as_string ().c_str ());

  if (auto lang_item = path.get_lang_prefix ())
    {
      NodeId seg_id
	= Analysis::Mappings::get ().get_lang_item_node (lang_item->first);

      insert_segment_resolution (Usage (lang_item->second), Definition (seg_id),
				 N);

      if (path.get_segments ().empty ())
	return Rib::Definition::NonShadowable (seg_id);

      auto new_start = stack.dfs_node (stack.root, seg_id);
      rust_assert (new_start.has_value ());
      starting_point = new_start.value ();

      can_descend = false;
    }
  else
    {
      switch (mode)
	{
	case ResolutionMode::Normal:
	  break; // default
	case ResolutionMode::FromRoot:
	  starting_point = stack.root;
	  break;
	case ResolutionMode::FromExtern:
	  starting_point = stack.extern_prelude;
	  break;
	default:
	  rust_unreachable ();
	}
    }

  if (path.get_segments ().empty ())
    return Rib::Definition::NonShadowable (starting_point.get ().id);

  auto &segments = path.get_segments ();

  // if there's only one segment, we just use `get`
  if (can_descend && segments.size () == 1)
    {
      auto &seg = segments.front ();

      tl::optional<Rib::Definition> res
	= stack.get (starting_point.get (), seg.name);

      if (!res)
	res = stack.get_lang_prelude (seg.name);

      if (N == Namespace::Types && !res)
	{
	  if (seg.is_crate_path_seg ())
	    {
	      insert_segment_resolution (Usage (seg.node_id),
					 Definition (stack.root.id), N);
	      // TODO: does NonShadowable matter?
	      return Rib::Definition::NonShadowable (stack.root.id);
	    }
	  else if (seg.is_lower_self_seg ())
	    {
	      NodeId id = stack.find_closest_module (starting_point.get ()).id;
	      insert_segment_resolution (Usage (seg.node_id), Definition (id),
					 N);
	      // TODO: does NonShadowable matter?
	      return Rib::Definition::NonShadowable (id);
	    }
	  else if (seg.is_super_path_seg ())
	    {
	      auto &closest_module
		= stack.find_closest_module (starting_point.get ());
	      if (closest_module.is_root ())
		{
		  rust_error_at (seg.locus, ErrorCode::E0433,
				 "too many leading %<super%> keywords");
		  return tl::nullopt;
		}

	      NodeId id
		= stack.find_closest_module (closest_module.parent.value ()).id;
	      insert_segment_resolution (Usage (seg.node_id), Definition (id),
					 N);
	      // TODO: does NonShadowable matter?
	      return Rib::Definition::NonShadowable (id);
	    }
	}

      // FIXME: Is the NS to insert_segment_resolution valid?
      if (res && !res->is_ambiguous ())
	insert_segment_resolution (Usage (seg.node_id),
				   Definition (res->get_node_id ()), N);
      return res;
    }

  auto iterator = segments.begin ();
  if (can_descend)
    {
      if (auto res = stack.find_starting_point (segments, starting_point,
						insert_segment_resolution,
						collect_errors))
	iterator = *res;
      else
	return tl::nullopt;
    }

  // We do the first part of path resolution exclusively in the types NS - this
  // gives us a node in which to resolve the last segment of the path.

  // nodes are shared between stacks
  auto node
    = resolve_segments (types, starting_point.get (), segments, iterator,
			insert_segment_resolution, collect_errors);

  if (!node)
    return tl::nullopt;

  // This node now represents the Node which *should* contain the definition
  // used by the last segment.
  auto &final_node = node.value ();

  // leave resolution within impl blocks to type checker
  if (final_node.rib (N).kind == Rib::Kind::TraitOrImpl)
    return tl::nullopt;

  auto &seg = segments.back ();
  std::string seg_name = seg.name;

  tl::optional<Rib::Definition> res
    = resolve_final_segment (stack, final_node, seg_name,
			     seg.is_lower_self_seg ());
  // Ok we didn't find it in the rib, Lets try the prelude...
  if (!res)
    res = stack.get_lang_prelude (seg_name);

  if (res && !res->is_ambiguous ())
    insert_segment_resolution (Usage (seg.node_id),
			       Definition (res->get_node_id ()), N);

  return res;
}

template <Namespace N>
tl::optional<typename ForeverStack<N>::Node &>
NameResolutionContext::resolve_segments (
  ForeverStack<N> &stack, typename ForeverStack<N>::Node &starting_point,
  const std::vector<ResolutionPath::Segment> &segments,
  typename ForeverStack<N>::SegIterator iterator,
  std::function<void (Usage, Definition, Namespace)> insert_segment_resolution,
  std::vector<Error> &collect_errors)
{
  auto *current_node = &starting_point;
  for (; !is_last (iterator, segments); iterator++)
    {
      auto &seg = *iterator;

      std::string str = seg.name;

      // check that we don't encounter *any* leading keywords afterwards
      if (check_leading_kw_at_start (collect_errors, seg,
				     seg.is_crate_path_seg ()
				       || seg.is_super_path_seg ()
				       || seg.is_lower_self_seg ()))
	return tl::nullopt;

      tl::optional<std::reference_wrapper<typename ForeverStack<N>::Node>> child
	= tl::nullopt;

      /*
       * On every iteration this loop either
       *
       * 1. terminates
       *
       * 2. decreases the depth of the node pointed to by current_node until
       *    current_node reaches the root
       *
       * 3. If the root node is reached, and we were not able to resolve the
       *    segment, we search the prelude rib for the segment, by setting
       *    current_node to point to the prelude, and toggling the
       *    searched_prelude boolean to true. If current_node is the prelude
       *    rib, and searched_prelude is true, we will exit.
       *
       * This ensures termination.
       *
       */
      bool searched_prelude = false;
      while (true)
	{
	  if (is_start (iterator, segments)
	      && current_node->rib (N).kind == Rib::Kind::TraitOrImpl)
	    {
	      // we can't reference associated types/functions like this
	      current_node = &current_node->parent.value ();
	      continue;
	    }

	  // may set the value of child
	  for (auto &kv : current_node->children)
	    {
	      auto &link = kv.first;

	      if (link.path.map_or (
		    [&str] (Identifier path) {
		      auto &path_str = path.as_string ();
		      return str == path_str;
		    },
		    false))
		{
		  child = kv.second;
		  break;
		}
	    }

	  if (child.has_value ())
	    {
	      break;
	    }

	  auto rib_lookup = current_node->rib (N).get (seg.name);
	  if (rib_lookup && !rib_lookup->is_ambiguous ())
	    {
	      if (Analysis::Mappings::get ()
		    .lookup_glob_container (rib_lookup->get_node_id ())
		    .has_value ())
		{
		  NodeId leaf_module
		    = stack.find_leaf_definition (rib_lookup->get_node_id ())
			.value_or (Definition (rib_lookup->get_node_id ()))
			.id;

		  if (auto new_child = stack.dfs_node (stack.root, leaf_module))
		    {
		      child = new_child.value ();
		      break;
		    }
		  else
		    {
		      return tl::nullopt;
		    }
		}
	      else
		{
		  // FIXME: Resolve segments always resolves in the Types NS
		  // correct? If so, we should remove the template parameter for
		  // this function - and use NS::Types directly here instead of
		  // N
		  insert_segment_resolution (Usage (seg.node_id),
					     Definition (
					       rib_lookup->get_node_id ()),
					     N);

		  return tl::nullopt;
		}
	    }

	  if (!searched_prelude
	      && should_search_prelude<N> (current_node, iterator, segments))
	    {
	      searched_prelude = true;
	      current_node = &stack.lang_prelude;
	      continue;
	    }

	  if (!is_start (iterator, segments)
	      || current_node->rib (N).kind == Rib::Kind::Module
	      || current_node->is_prelude ())
	    {
	      return tl::nullopt;
	    }

	  current_node = &current_node->parent.value ();
	}

      // if child didn't point to a value
      // the while loop above would have returned or kept looping
      current_node = &child->get ();
      insert_segment_resolution (Usage (seg.node_id),
				 Definition (current_node->id), N);
    }

  return *current_node;
}

template <>
inline tl::optional<Rib::Definition>
NameResolutionContext::resolve_final_segment (
  ForeverStack<Namespace::Types> &stack,
  typename ForeverStack<Namespace::Types>::Node &final_node,
  std::string &seg_name, bool is_lower_self)
{
  if (is_lower_self)
    return Rib::Definition::NonShadowable (final_node.id);
  else
    return final_node.rib_types.get (seg_name);
}

template <Namespace N>
tl::optional<Rib::Definition>
NameResolutionContext::resolve_final_segment (
  ForeverStack<N> &stack, typename ForeverStack<N>::Node &final_node,
  std::string &seg_name, bool is_lower_self)
{
  return final_node.rib (N).get (seg_name);
}

} // namespace Resolver2_0
} // namespace Rust
