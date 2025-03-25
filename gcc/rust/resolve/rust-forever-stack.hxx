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

#include "expected.h"
#include "rust-ast.h"
#include "rust-diagnostics.h"
#include "rust-forever-stack.h"
#include "rust-edition.h"
#include "rust-rib.h"
#include "rust-unwrap-segment.h"
#include "optional.h"

namespace Rust {
namespace Resolver2_0 {

template <Namespace N>
bool
ForeverStack<N>::Node::is_root () const
{
  return !parent.has_value ();
}

template <Namespace N>
bool
ForeverStack<N>::Node::is_prelude () const
{
  return rib.kind == Rib::Kind::Prelude;
}

template <Namespace N>
bool
ForeverStack<N>::Node::is_leaf () const
{
  return children.empty ();
}

template <Namespace N>
void
ForeverStack<N>::Node::insert_child (Link link, Node child)
{
  auto res = children.insert ({link, child});

  // Do we want to error if the child already exists? Probably not, right?
  // That's kinda the point, isn't it. So this method always succeeds, right?
}

template <Namespace N>
void
ForeverStack<N>::push (Rib::Kind rib_kind, NodeId id,
		       tl::optional<Identifier> path)
{
  push_inner (rib_kind, Link (id, path));
}

template <Namespace N>
void
ForeverStack<N>::push_inner (Rib rib, Link link)
{
  if (rib.kind == Rib::Kind::Prelude)
    {
      // If you push_inner into the prelude from outside the root, you will pop
      // back into the root, which could screw up a traversal.
      rust_assert (&cursor_reference.get () == &root);
      // Prelude doesn't have an access path
      rust_assert (!link.path);
      update_cursor (this->lang_prelude);
      return;
    }
  // If the link does not exist, we create it and emplace a new `Node` with the
  // current node as its parent. `unordered_map::emplace` returns a pair with
  // the iterator and a boolean. If the value already exists, the iterator
  // points to it. Otherwise, it points to the newly emplaced value, so we can
  // just update our cursor().
  auto emplace = cursor ().children.emplace (
    std::make_pair (link, Node (rib, link.id, cursor ())));

  auto it = emplace.first;
  auto existed = !emplace.second;

  rust_debug ("inserting link: Link(%d [%s]): existed? %s", link.id,
	      link.path.has_value () ? link.path.value ().as_string ().c_str ()
				     : "<anon>",
	      existed ? "yes" : "no");

  // We update the cursor
  update_cursor (it->second);
}

template <Namespace N>
void
ForeverStack<N>::pop ()
{
  rust_assert (!cursor ().is_root ());

  rust_debug ("popping link");

  for (const auto &kv : cursor ().rib.get_values ())
    rust_debug ("current_rib: k: %s, v: %s", kv.first.c_str (),
		kv.second.to_string ().c_str ());

  if (cursor ().parent.has_value ())
    for (const auto &kv : cursor ().parent.value ().rib.get_values ())
      rust_debug ("new cursor: k: %s, v: %s", kv.first.c_str (),
		  kv.second.to_string ().c_str ());

  update_cursor (cursor ().parent.value ());
}

static tl::expected<NodeId, DuplicateNameError>
insert_inner (Rib &rib, std::string name, Rib::Definition definition)
{
  return rib.insert (name, definition);
}

template <Namespace N>
tl::expected<NodeId, DuplicateNameError>
ForeverStack<N>::insert (Identifier name, NodeId node)
{
  auto &innermost_rib = peek ();

  // So what do we do here - if the Rib has already been pushed in an earlier
  // pass, we might end up in a situation where it is okay to re-add new names.
  // Do we just ignore that here? Do we keep track of if the Rib is new or not?
  // should our cursor have info on the current node like "is it newly pushed"?
  return insert_inner (innermost_rib, name.as_string (),
		       Rib::Definition::NonShadowable (node));
}

template <Namespace N>
tl::expected<NodeId, DuplicateNameError>
ForeverStack<N>::insert_shadowable (Identifier name, NodeId node)
{
  auto &innermost_rib = peek ();

  return insert_inner (innermost_rib, name.as_string (),
		       Rib::Definition::Shadowable (node));
}

template <Namespace N>
tl::expected<NodeId, DuplicateNameError>
ForeverStack<N>::insert_globbed (Identifier name, NodeId node)
{
  auto &innermost_rib = peek ();

  return insert_inner (innermost_rib, name.as_string (),
		       Rib::Definition::Globbed (node));
}

template <Namespace N>
tl::expected<NodeId, DuplicateNameError>
ForeverStack<N>::insert_at_root (Identifier name, NodeId node)
{
  auto &root_rib = root.rib;

  // inserting in the root of the crate is never a shadowing operation, even for
  // macros
  return insert_inner (root_rib, name.as_string (),
		       Rib::Definition::NonShadowable (node));
}

// Specialization for Macros and Labels - where we are allowed to shadow
// existing definitions
template <>
inline tl::expected<NodeId, DuplicateNameError>
ForeverStack<Namespace::Macros>::insert (Identifier name, NodeId node)
{
  return insert_inner (peek (), name.as_string (),
		       Rib::Definition::Shadowable (node));
}

template <>
inline tl::expected<NodeId, DuplicateNameError>
ForeverStack<Namespace::Labels>::insert (Identifier name, NodeId node)
{
  return insert_inner (peek (), name.as_string (),
		       Rib::Definition::Shadowable (node));
}

template <>
inline tl::expected<NodeId, DuplicateNameError>
ForeverStack<Namespace::Types>::insert_variant (Identifier name, NodeId node)
{
  return insert_inner (peek (), name.as_string (),
		       Rib::Definition::NonShadowable (node, true));
}

template <Namespace N>
Rib &
ForeverStack<N>::peek ()
{
  return cursor ().rib;
}

template <Namespace N>
const Rib &
ForeverStack<N>::peek () const
{
  return cursor ().rib;
}

template <Namespace N>
void
ForeverStack<N>::reverse_iter (std::function<KeepGoing (Node &)> lambda)
{
  return reverse_iter (cursor (), lambda);
}

template <Namespace N>
void
ForeverStack<N>::reverse_iter (
  std::function<KeepGoing (const Node &)> lambda) const
{
  return reverse_iter (cursor (), lambda);
}

template <Namespace N>
void
ForeverStack<N>::reverse_iter (Node &start,
			       std::function<KeepGoing (Node &)> lambda)
{
  auto *tmp = &start;

  while (true)
    {
      auto keep_going = lambda (*tmp);
      if (keep_going == KeepGoing::No)
	return;

      if (tmp->is_root ())
	return;

      tmp = &tmp->parent.value ();
    }
}

template <Namespace N>
void
ForeverStack<N>::reverse_iter (
  const Node &start, std::function<KeepGoing (const Node &)> lambda) const
{
  auto *tmp = &start;

  while (true)
    {
      auto keep_going = lambda (*tmp);
      if (keep_going == KeepGoing::No)
	return;

      if (tmp->is_root ())
	return;

      tmp = &tmp->parent.value ();
    }
}

template <Namespace N>
typename ForeverStack<N>::Node &
ForeverStack<N>::cursor ()
{
  return cursor_reference;
}

template <Namespace N>
const typename ForeverStack<N>::Node &
ForeverStack<N>::cursor () const
{
  return cursor_reference;
}

template <Namespace N>
void
ForeverStack<N>::update_cursor (Node &new_cursor)
{
  cursor_reference = new_cursor;
}

template <Namespace N>
tl::optional<Rib::Definition>
ForeverStack<N>::get (const Identifier &name)
{
  tl::optional<Rib::Definition> resolved_definition = tl::nullopt;

  // TODO: Can we improve the API? have `reverse_iter` return an optional?
  reverse_iter ([&resolved_definition, &name] (Node &current) {
    auto candidate = current.rib.get (name.as_string ());

    return candidate.map_or (
      [&resolved_definition] (Rib::Definition found) {
	if (found.is_variant ())
	  return KeepGoing::Yes;
	// for most namespaces, we do not need to care about various ribs -
	// they are available from all contexts if defined in the current
	// scope, or an outermore one. so if we do have a candidate, we can
	// return it directly and stop iterating
	resolved_definition = found;

	return KeepGoing::No;
      },
      // if there was no candidate, we keep iterating
      KeepGoing::Yes);
  });

  return resolved_definition;
}

template <Namespace N>
tl::optional<Rib::Definition>
ForeverStack<N>::get_lang_prelude (const Identifier &name)
{
  return lang_prelude.rib.get (name.as_string ());
}

template <Namespace N>
tl::optional<Rib::Definition>
ForeverStack<N>::get_lang_prelude (const std::string &name)
{
  return lang_prelude.rib.get (name);
}

template <>
tl::optional<Rib::Definition> inline ForeverStack<Namespace::Labels>::get (
  const Identifier &name)
{
  tl::optional<Rib::Definition> resolved_definition = tl::nullopt;

  reverse_iter ([&resolved_definition, &name] (Node &current) {
    // looking up for labels cannot go through function ribs
    // TODO: What other ribs?
    if (current.rib.kind == Rib::Kind::Function)
      return KeepGoing::No;

    auto candidate = current.rib.get (name.as_string ());

    // FIXME: Factor this in a function with the generic `get`
    return candidate.map_or (
      [&resolved_definition] (Rib::Definition found) {
	resolved_definition = found;

	return KeepGoing::No;
      },
      KeepGoing::Yes);
  });

  return resolved_definition;
}

/* Check if an iterator points to the last element */
template <typename I, typename C>
static bool
is_last (const I &iterator, const C &collection)
{
  return iterator + 1 == collection.end ();
}

/* Check if an iterator points to the start of the collection */
template <typename I, typename C>
static bool
is_start (const I &iterator, const C &collection)
{
  return iterator == collection.begin ();
}

template <Namespace N>
typename ForeverStack<N>::Node &
ForeverStack<N>::find_closest_module (Node &starting_point)
{
  auto *closest_module = &starting_point;

  reverse_iter (starting_point, [&closest_module] (Node &current) {
    if (current.rib.kind == Rib::Kind::Module || current.is_root ())
      {
	closest_module = &current;
	return KeepGoing::No;
      }

    return KeepGoing::Yes;
  });

  return *closest_module;
}

/* If a the given condition is met, emit an error about misused leading path
 * segments */
template <typename S>
static inline bool
check_leading_kw_at_start (const S &segment, bool condition)
{
  if (condition)
    rust_error_at (
      segment.get_locus (), ErrorCode::E0433,
      "leading path segment %qs can only be used at the beginning of a path",
      segment.as_string ().c_str ());

  return condition;
}

// we first need to handle the "starting" segments - `super`, `self` or
// `crate`. we don't need to do anything for `self` and can just skip it. for
// `crate`, we need to go back to the root of the current stack. for each
// `super` segment, we go back to the cursor's parent until we reach the
// correct one or the root.
template <Namespace N>
template <typename S>
tl::optional<typename std::vector<S>::const_iterator>
ForeverStack<N>::find_starting_point (
  const std::vector<S> &segments, std::reference_wrapper<Node> &starting_point,
  std::function<void (const S &, NodeId)> insert_segment_resolution)
{
  auto iterator = segments.begin ();

  for (; !is_last (iterator, segments); iterator++)
    {
      auto &outer_seg = *iterator;

      if (unwrap_segment_get_lang_item (outer_seg).has_value ())
	break;

      auto &seg = unwrap_type_segment (outer_seg);
      bool is_self_or_crate
	= seg.is_crate_path_seg () || seg.is_lower_self_seg ();

      // if we're after the first path segment and meet `self` or `crate`, it's
      // an error - we should only be seeing `super` keywords at this point
      if (check_leading_kw_at_start (seg, !is_start (iterator, segments)
					    && is_self_or_crate))
	return tl::nullopt;

      if (seg.is_crate_path_seg ())
	{
	  starting_point = root;
	  insert_segment_resolution (outer_seg, starting_point.get ().id);
	  iterator++;
	  break;
	}
      if (seg.is_lower_self_seg ())
	{
	  // insert segment resolution and exit
	  starting_point = find_closest_module (starting_point);
	  insert_segment_resolution (outer_seg, starting_point.get ().id);
	  iterator++;
	  break;
	}
      if (seg.is_super_path_seg ())
	{
	  starting_point = find_closest_module (starting_point);
	  if (starting_point.get ().is_root ())
	    {
	      rust_error_at (seg.get_locus (), ErrorCode::E0433,
			     "too many leading %<super%> keywords");
	      return tl::nullopt;
	    }

	  starting_point
	    = find_closest_module (starting_point.get ().parent.value ());

	  insert_segment_resolution (outer_seg, starting_point.get ().id);
	  continue;
	}

      // now we've gone through the allowed `crate`, `self` or leading `super`
      // segments. we can start resolving each segment itself.
      // if we do see another leading segment, then we can error out.
      break;
    }

  return iterator;
}

template <Namespace N>
template <typename S>
tl::optional<typename ForeverStack<N>::Node &>
ForeverStack<N>::resolve_segments (
  Node &starting_point, const std::vector<S> &segments,
  typename std::vector<S>::const_iterator iterator,
  std::function<void (const S &, NodeId)> insert_segment_resolution)
{
  Node *current_node = &starting_point;
  for (; !is_last (iterator, segments); iterator++)
    {
      auto &outer_seg = *iterator;

      if (auto lang_item = unwrap_segment_get_lang_item (outer_seg))
	{
	  NodeId seg_id = Analysis::Mappings::get ().get_lang_item_node (
	    lang_item.value ());
	  current_node = &dfs_node (root, seg_id).value ();

	  insert_segment_resolution (outer_seg, seg_id);
	  continue;
	}

      auto &seg = unwrap_type_segment (outer_seg);
      std::string str = seg.as_string ();
      rust_debug ("[ARTHUR]: resolving segment part: %s", str.c_str ());

      // check that we don't encounter *any* leading keywords afterwards
      if (check_leading_kw_at_start (seg, seg.is_crate_path_seg ()
					    || seg.is_super_path_seg ()
					    || seg.is_lower_self_seg ()))
	return tl::nullopt;

      tl::optional<typename ForeverStack<N>::Node &> child = tl::nullopt;

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

	  if (N == Namespace::Types)
	    {
	      auto rib_lookup = current_node->rib.get (seg.as_string ());
	      if (rib_lookup && !rib_lookup->is_ambiguous ())
		{
		  insert_segment_resolution (outer_seg,
					     rib_lookup->get_node_id ());
		  return tl::nullopt;
		}
	    }

	  if (current_node->is_root () && !searched_prelude)
	    {
	      searched_prelude = true;
	      current_node = &lang_prelude;
	      continue;
	    }

	  if (!is_start (iterator, segments)
	      || current_node->rib.kind == Rib::Kind::Module
	      || current_node->is_prelude ())
	    {
	      return tl::nullopt;
	    }

	  current_node = &current_node->parent.value ();
	}

      // if child didn't contain a value
      // the while loop above should have return'd or kept looping
      current_node = &child.value ();
      insert_segment_resolution (outer_seg, current_node->id);
    }

  return *current_node;
}

template <>
inline tl::optional<Rib::Definition>
ForeverStack<Namespace::Types>::resolve_final_segment (Node &final_node,
						       std::string &seg_name,
						       bool is_lower_self)
{
  if (is_lower_self)
    return Rib::Definition::NonShadowable (final_node.id);
  else
    return final_node.rib.get (seg_name);
}

template <Namespace N>
tl::optional<Rib::Definition>
ForeverStack<N>::resolve_final_segment (Node &final_node, std::string &seg_name,
					bool is_lower_self)
{
  return final_node.rib.get (seg_name);
}

template <Namespace N>
template <typename S>
tl::optional<Rib::Definition>
ForeverStack<N>::resolve_path (
  const std::vector<S> &segments, bool has_opening_scope_resolution,
  std::function<void (const S &, NodeId)> insert_segment_resolution)
{
  // TODO: What to do if segments.empty() ?

  // handle paths with opening scopes
  std::function<void (void)> cleanup_current = [] () {};
  if (has_opening_scope_resolution)
    {
      Node *last_current = &cursor_reference.get ();
      if (get_rust_edition () == Edition::E2015)
	cursor_reference = root;
      else
	cursor_reference = extern_prelude;
      cleanup_current
	= [this, last_current] () { cursor_reference = *last_current; };
    }

  // if there's only one segment, we just use `get`
  if (segments.size () == 1)
    {
      auto &seg = segments.front ();
      if (auto lang_item = unwrap_segment_get_lang_item (seg))
	{
	  NodeId seg_id = Analysis::Mappings::get ().get_lang_item_node (
	    lang_item.value ());

	  insert_segment_resolution (seg, seg_id);
	  cleanup_current ();
	  // TODO: does NonShadowable matter?
	  return Rib::Definition::NonShadowable (seg_id);
	}

      tl::optional<Rib::Definition> res
	= get (unwrap_type_segment (segments.back ()).as_string ());

      if (!res)
	res = get_lang_prelude (
	  unwrap_type_segment (segments.back ()).as_string ());

      if (res && !res->is_ambiguous ())
	insert_segment_resolution (segments.back (), res->get_node_id ());
      cleanup_current ();
      return res;
    }

  std::reference_wrapper<Node> starting_point = cursor ();

  auto res
    = find_starting_point (segments, starting_point, insert_segment_resolution)
	.and_then (
	  [this, &segments, &starting_point, &insert_segment_resolution] (
	    typename std::vector<S>::const_iterator iterator) {
	    return resolve_segments (starting_point.get (), segments, iterator,
				     insert_segment_resolution);
	  })
	.and_then ([this, &segments, &insert_segment_resolution] (
		     Node final_node) -> tl::optional<Rib::Definition> {
	  // leave resolution within impl blocks to type checker
	  if (final_node.rib.kind == Rib::Kind::TraitOrImpl)
	    return tl::nullopt;

	  auto &seg = unwrap_type_segment (segments.back ());
	  std::string seg_name = seg.as_string ();

	  // assuming this can't be a lang item segment
	  tl::optional<Rib::Definition> res
	    = resolve_final_segment (final_node, seg_name,
				     seg.is_lower_self_seg ());
	  // Ok we didn't find it in the rib, Lets try the prelude...
	  if (!res)
	    res = get_lang_prelude (seg_name);

	  if (res && !res->is_ambiguous ())
	    insert_segment_resolution (segments.back (), res->get_node_id ());

	  return res;
	});
  cleanup_current ();
  return res;
}

template <Namespace N>
tl::optional<typename ForeverStack<N>::DfsResult>
ForeverStack<N>::dfs (ForeverStack<N>::Node &starting_point, NodeId to_find)
{
  auto values = starting_point.rib.get_values ();

  for (auto &kv : values)
    {
      for (auto id : kv.second.ids_shadowable)
	if (id == to_find)
	  return {{starting_point, kv.first}};
      for (auto id : kv.second.ids_non_shadowable)
	if (id == to_find)
	  return {{starting_point, kv.first}};
      for (auto id : kv.second.ids_globbed)
	if (id == to_find)
	  return {{starting_point, kv.first}};
    }

  for (auto &child : starting_point.children)
    {
      auto candidate = dfs (child.second, to_find);

      if (candidate.has_value ())
	return candidate;
    }

  return tl::nullopt;
}

template <Namespace N>
tl::optional<typename ForeverStack<N>::ConstDfsResult>
ForeverStack<N>::dfs (const ForeverStack<N>::Node &starting_point,
		      NodeId to_find) const
{
  auto values = starting_point.rib.get_values ();

  for (auto &kv : values)
    {
      for (auto id : kv.second.ids_shadowable)
	if (id == to_find)
	  return {{starting_point, kv.first}};
      for (auto id : kv.second.ids_non_shadowable)
	if (id == to_find)
	  return {{starting_point, kv.first}};
      for (auto id : kv.second.ids_globbed)
	if (id == to_find)
	  return {{starting_point, kv.first}};
    }

  for (auto &child : starting_point.children)
    {
      auto candidate = dfs (child.second, to_find);

      if (candidate.has_value ())
	return candidate;
    }

  return tl::nullopt;
}

template <Namespace N>
tl::optional<Resolver::CanonicalPath>
ForeverStack<N>::to_canonical_path (NodeId id) const
{
  // find the id in the current forever stack, starting from the root,
  // performing either a BFS or DFS once the Node containing the ID is found, go
  // back up to the root (parent().parent().parent()...) accumulate link
  // segments reverse them that's your canonical path

  return dfs (root, id).map ([this, id] (ConstDfsResult tuple) {
    auto containing_node = tuple.first;
    auto name = tuple.second;

    auto segments = std::vector<Resolver::CanonicalPath> ();

    reverse_iter (containing_node, [&segments] (const Node &current) {
      if (current.is_root ())
	return KeepGoing::No;

      auto children = current.parent.value ().children;
      const Link *outer_link = nullptr;

      for (auto &kv : children)
	{
	  auto &link = kv.first;
	  auto &child = kv.second;

	  if (current.id == child.id)
	    {
	      outer_link = &link;
	      break;
	    }
	}

      rust_assert (outer_link);

      outer_link->path.map ([&segments, outer_link] (Identifier path) {
	segments.emplace (segments.begin (),
			  Resolver::CanonicalPath::new_seg (outer_link->id,
							    path.as_string ()));
      });

      return KeepGoing::Yes;
    });

    auto &mappings = Analysis::Mappings::get ();
    CrateNum crate_num = mappings.lookup_crate_num (root.id).value ();
    auto path = Resolver::CanonicalPath::new_seg (
      root.id, mappings.get_crate_name (crate_num).value ());
    path.set_crate_num (crate_num);

    for (const auto &segment : segments)
      path = path.append (segment);

    // Finally, append the name
    path = path.append (Resolver::CanonicalPath::new_seg (id, name));

    return path;
  });
}

template <Namespace N>
tl::optional<Rib &>
ForeverStack<N>::dfs_rib (ForeverStack<N>::Node &starting_point, NodeId to_find)
{
  return dfs_node (starting_point, to_find).map ([] (Node &x) -> Rib & {
    return x.rib;
  });
}

template <Namespace N>
tl::optional<const Rib &>
ForeverStack<N>::dfs_rib (const ForeverStack<N>::Node &starting_point,
			  NodeId to_find) const
{
  return dfs_node (starting_point, to_find)
    .map ([] (const Node &x) -> const Rib & { return x.rib; });
}

template <Namespace N>
tl::optional<typename ForeverStack<N>::Node &>
ForeverStack<N>::dfs_node (ForeverStack<N>::Node &starting_point,
			   NodeId to_find)
{
  if (starting_point.id == to_find)
    return starting_point;

  for (auto &child : starting_point.children)
    {
      auto candidate = dfs_node (child.second, to_find);

      if (candidate.has_value ())
	return candidate;
    }

  return tl::nullopt;
}

template <Namespace N>
tl::optional<const typename ForeverStack<N>::Node &>
ForeverStack<N>::dfs_node (const ForeverStack<N>::Node &starting_point,
			   NodeId to_find) const
{
  if (starting_point.id == to_find)
    return starting_point;

  for (auto &child : starting_point.children)
    {
      auto candidate = dfs_node (child.second, to_find);

      if (candidate.has_value ())
	return candidate;
    }

  return tl::nullopt;
}

template <Namespace N>
tl::optional<Rib &>
ForeverStack<N>::to_rib (NodeId rib_id)
{
  return dfs_rib (root, rib_id);
}

template <Namespace N>
tl::optional<const Rib &>
ForeverStack<N>::to_rib (NodeId rib_id) const
{
  return dfs_rib (root, rib_id);
}

template <Namespace N>
void
ForeverStack<N>::stream_rib (std::stringstream &stream, const Rib &rib,
			     const std::string &next,
			     const std::string &next_next) const
{
  std::string rib_kind = Rib::kind_to_string (rib.kind);
  stream << next << "rib [" << rib_kind << "]: {";
  if (rib.get_values ().empty ())
    {
      stream << "}\n";
      return;
    }
  else
    {
      stream << "\n";
    }

  for (const auto &kv : rib.get_values ())
    stream << next_next << kv.first << ": " << kv.second.to_string () << "\n";

  stream << next << "},\n";
}

template <Namespace N>
void
ForeverStack<N>::stream_node (std::stringstream &stream, unsigned indentation,
			      const ForeverStack<N>::Node &node) const
{
  auto indent = std::string (indentation, ' ');
  auto next = std::string (indentation + 4, ' ');
  auto next_next = std::string (indentation + 8, ' ');

  stream << indent << "Node {\n"
	 << next << "is_root: " << (node.is_root () ? "true" : "false") << ",\n"
	 << next << "is_leaf: " << (node.is_leaf () ? "true" : "false")
	 << ",\n";

  stream_rib (stream, node.rib, next, next_next);

  stream << indent << "}\n";

  for (auto &kv : node.children)
    {
      auto link = kv.first;
      auto child = kv.second;
      stream << indent << "Link (" << link.id << ", "
	     << (link.path.has_value () ? link.path.value ().as_string ()
					: "<anon>")
	     << "):\n";

      stream_node (stream, indentation + 4, child);

      stream << '\n';
    }
}

template <Namespace N>
std::string
ForeverStack<N>::as_debug_string () const
{
  std::stringstream stream;

  stream_node (stream, 0, root);

  return stream.str ();
}

template <Namespace N>
bool
ForeverStack<N>::is_module_descendant (NodeId parent, NodeId child) const
{
  return dfs_node (dfs_node (root, parent).value (), child).has_value ();
}

// FIXME: Can we add selftests?

} // namespace Resolver2_0
} // namespace Rust
