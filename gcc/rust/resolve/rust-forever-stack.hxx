// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

bool
ForeverStackBase::Node::is_root () const
{
  return !parent.has_value ();
}

bool
ForeverStackBase::Node::is_prelude () const
{
  return rib_values.kind == Rib::Kind::Prelude;
}

bool
ForeverStackBase::Node::is_leaf () const
{
  return children.empty ();
}

void
ForeverStackBase::Node::insert_child (Link link, Node child)
{
  children.insert ({link, child});

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
ForeverStack<N>::push_inner (Rib::Kind rib_kind, Link link)
{
  if (rib_kind == Rib::Kind::Prelude)
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
    std::make_pair (link, Node (rib_kind, link.id, cursor ())));

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

  for (const auto &kv : cursor ().rib (N).get_values ())
    rust_debug ("current_rib: k: %s, v: %s", kv.first.c_str (),
		kv.second.to_string ().c_str ());

  if (cursor ().parent.has_value ())
    for (const auto &kv : cursor ().parent.value ().rib (N).get_values ())
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
  auto &root_rib = root.rib (N);

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

template <>
inline tl::expected<NodeId, DuplicateNameError>
ForeverStack<Namespace::Values>::insert_variant (Identifier name, NodeId node)
{
  return insert_inner (peek (), name.as_string (),
		       Rib::Definition::NonShadowable (node, true));
}

template <Namespace N>
inline void
ForeverStack<N>::insert_lang_prelude (Identifier name, NodeId id)
{
  insert_inner (lang_prelude.rib (N), name.as_string (),
		Rib::Definition::NonShadowable (id, false));
}

template <Namespace N>
Rib &
ForeverStack<N>::peek ()
{
  return cursor ().rib (N);
}

template <Namespace N>
const Rib &
ForeverStack<N>::peek () const
{
  return cursor ().rib (N);
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
ForeverStack<N>::get (Node &start, const Identifier &name)
{
  tl::optional<Rib::Definition> resolved_definition = tl::nullopt;

  // TODO: Can we improve the API? have `reverse_iter` return an optional?
  reverse_iter (start, [&resolved_definition, &name] (Node &current) {
    // we can't reference associated types/functions like this
    if (current.rib (N).kind == Rib::Kind::TraitOrImpl)
      return KeepGoing::Yes;

    auto candidate = current.rib (N).get (name.as_string ());

    if (candidate)
      {
	if (candidate->is_variant ())
	  return KeepGoing::Yes;
	// for most namespaces, we do not need to care about various ribs -
	// they are available from all contexts if defined in the current
	// scope, or an outermore one. so if we do have a candidate, we can
	// return it directly and stop iterating
	resolved_definition = *candidate;

	return KeepGoing::No;
      }
    else
      {
	if (current.rib (N).kind == Rib::Kind::Module)
	  return KeepGoing::No;
	else
	  return KeepGoing::Yes;
      }
  });

  return resolved_definition;
}

template <Namespace N>
tl::optional<Rib::Definition>
ForeverStack<N>::get (const Identifier &name)
{
  return get (cursor (), name);
}

template <Namespace N>
tl::optional<Rib::Definition>
ForeverStack<N>::get_lang_prelude (const Identifier &name)
{
  return lang_prelude.rib (N).get (name.as_string ());
}

template <Namespace N>
tl::optional<Rib::Definition>
ForeverStack<N>::get_lang_prelude (const std::string &name)
{
  return lang_prelude.rib (N).get (name);
}

template <Namespace N>
tl::optional<Rib::Definition>
ForeverStack<N>::get_from_prelude (NodeId prelude, const Identifier &name)
{
  auto starting_point = dfs_node (root, prelude);
  if (!starting_point)
    return tl::nullopt;

  return get (*starting_point, name);
}

template <>
tl::optional<Rib::Definition> inline ForeverStack<Namespace::Labels>::get (
  const Identifier &name)
{
  tl::optional<Rib::Definition> resolved_definition = tl::nullopt;

  reverse_iter ([&resolved_definition, &name] (Node &current) {
    // looking up for labels cannot go through function ribs
    // TODO: What other ribs?
    if (current.rib_labels.kind == Rib::Kind::Function)
      return KeepGoing::No;

    auto candidate = current.rib_labels.get (name.as_string ());

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
    if (current.rib (N).kind == Rib::Kind::Module || current.is_root ())
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
static inline bool
check_leading_kw_at_start (std::vector<Error> &collect_errors,
			   const ResolutionPath::Segment &segment,
			   bool condition)
{
  if (condition)
    collect_errors.emplace_back (
      segment.locus, ErrorCode::E0433,
      "%qs in paths can only be used in start position", segment.name.c_str ());

  return condition;
}

// we first need to handle the "starting" segments - `super`, `self` or
// `crate`. we don't need to do anything for `self` and can just skip it. for
// `crate`, we need to go back to the root of the current stack. for each
// `super` segment, we go back to the cursor's parent until we reach the
// correct one or the root.
template <Namespace N>
tl::optional<typename std::vector<ResolutionPath::Segment>::const_iterator>
ForeverStack<N>::find_starting_point (
  const std::vector<ResolutionPath::Segment> &segments,
  std::reference_wrapper<Node> &starting_point,
  std::function<void (Usage, Definition, Namespace)> insert_segment_resolution,
  std::vector<Error> &collect_errors)
{
  auto iterator = segments.begin ();

  for (; iterator != segments.end (); iterator++)
    {
      auto &seg = *iterator;

      // don't include a final self segment
      if (is_last (iterator, segments) && seg.is_lower_self_seg ())
	break;

      bool is_self_or_crate
	= seg.is_crate_path_seg () || seg.is_lower_self_seg ();

      // if we're after the first path segment and meet `self` or `crate`, it's
      // an error - we should only be seeing `super` keywords at this point
      if (check_leading_kw_at_start (collect_errors, seg,
				     !is_start (iterator, segments)
				       && is_self_or_crate))
	return tl::nullopt;

      if (seg.is_crate_path_seg ())
	{
	  starting_point = root;
	  insert_segment_resolution (Usage (seg.node_id),
				     Definition (starting_point.get ().id), N);
	  iterator++;
	  break;
	}
      if (seg.is_lower_self_seg ())
	{
	  // insert segment resolution
	  starting_point = find_closest_module (starting_point);
	  insert_segment_resolution (Usage (seg.node_id),
				     Definition (starting_point.get ().id), N);
	  // don't exit -- we could see some "super" segments
	  continue;
	}
      if (seg.is_super_path_seg ())
	{
	  starting_point = find_closest_module (starting_point);
	  if (starting_point.get ().is_root ())
	    {
	      collect_errors.emplace_back (
		seg.locus, ErrorCode::E0433,
		"too many leading %<super%> keywords");
	      return tl::nullopt;
	    }

	  starting_point
	    = find_closest_module (starting_point.get ().parent.value ());

	  insert_segment_resolution (Usage (seg.node_id),
				     Definition (starting_point.get ().id), N);
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
tl::optional<typename ForeverStack<N>::DfsResult>
ForeverStack<N>::dfs (ForeverStack<N>::Node &starting_point, NodeId to_find)
{
  auto values = starting_point.rib (N).get_values ();

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
  auto values = starting_point.rib (N).get_values ();

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
tl::optional<Rib &>
ForeverStack<N>::dfs_rib (ForeverStack<N>::Node &starting_point, NodeId to_find)
{
  return dfs_node (starting_point, to_find).map ([] (Node &x) -> Rib & {
    return x.rib (N);
  });
}

template <Namespace N>
tl::optional<const Rib &>
ForeverStack<N>::dfs_rib (const ForeverStack<N>::Node &starting_point,
			  NodeId to_find) const
{
  return dfs_node (starting_point, to_find)
    .map ([] (const Node &x) -> const Rib & { return x.rib (N); });
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
			      const ForeverStack<N>::Node &node,
			      unsigned depth) const
{
  auto indent = std::string (indentation, ' ');
  auto next = std::string (indentation + 4, ' ');
  auto next_next = std::string (indentation + 8, ' ');

  stream << indent << "Node {\n"
	 << next << "is_root: " << (node.is_root () ? "true" : "false") << ",\n"
	 << next << "is_leaf: " << (node.is_leaf () ? "true" : "false")
	 << ",\n";

  stream_rib (stream, node.rib (N), next, next_next);

  stream << indent << "}\n";

  for (auto &kv : node.children)
    {
      auto link = kv.first;
      auto child = kv.second;
      stream << indent << "Link " << depth << " (" << link.id << ", "
	     << (link.path.has_value () ? link.path.value ().as_string ()
					: "<anon>")
	     << "):\n";

      stream_node (stream, indentation + 4, child, depth + 1);

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

static tl::expected<Definition, LookupFinalizeError>
find_leaf_definition_inner (const Usage &key,
			    const std::map<Usage, Definition> &resolved_nodes,
			    std::set<Usage> &keys_seen)
{
  auto original_definition = resolved_nodes.find (key);
  auto possible_import = Usage (original_definition->second.id);

  if (original_definition == resolved_nodes.end ())
    return tl::make_unexpected (LookupFinalizeError::NoDefinition);

  if (!keys_seen.insert (key).second)
    return tl::make_unexpected (LookupFinalizeError::Loop);

  if (resolved_nodes.find (possible_import) == resolved_nodes.end ())
    return original_definition->second;

  // We're dealing with an import - a reference to another
  // definition. Go through the chain and update the original key's
  // corresponding definition.
  return find_leaf_definition_inner (possible_import, resolved_nodes,
				     keys_seen);
}

template <Namespace N>
tl::expected<Definition, LookupFinalizeError>
ForeverStack<N>::find_leaf_definition (const NodeId &key) const
{
  std::set<Usage> keys_seen;

  return find_leaf_definition_inner (Usage (key), resolved_nodes, keys_seen);
}

#if 0
template <Namespace N>
void
ForeverStack<N>::flatten ()
{
  for (auto &k_v : resolved_nodes)
    {
      // Loop detection
      auto keys_seen = std::set<Usage> ();

      auto result
	= find_leaf_definition_inner (k_v.first, resolved_nodes, keys_seen);

      if (!result)
	{
	  // Trigger an ICE if we haven't found a definition because that's
	  // really weird
	  rust_assert (result.error () != LookupFinalizeError::NoDefinition);

	  // FIXME: This needs to be improved and tested, but later
	  rust_error_at (UNDEF_LOCATION, "import loop");
	  continue;
	}

      // Replace the Definition for this Usage in the map. This may be a no-op.
      k_v.second = result.value ();
    }
}
#endif

// FIXME: Can we add selftests?

} // namespace Resolver2_0
} // namespace Rust
