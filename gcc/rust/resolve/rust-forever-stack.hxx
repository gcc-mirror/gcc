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

#include "rust-forever-stack.h"
#include "rust-rib.h"
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

// FIXME: Add correct implementation
template <Namespace N>
void
ForeverStack<N>::push (Rib rib, NodeId id, tl::optional<Identifier> path)
{
  push_inner (rib, Link (id, path));
}

template <Namespace N>
void
ForeverStack<N>::push_inner (Rib rib, Link link)
{
  // If the link does not exist, we create it and emplace a new `Node` with the
  // current node as its parent. `unordered_map::emplace` returns a pair with
  // the iterator and a boolean. If the value already exists, the iterator
  // points to it. Otherwise, it points to the newly emplaced value, so we can
  // just update our cursor().
  auto emplace
    = cursor ().children.emplace (std::make_pair (link, Node (rib, cursor ())));

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
    rust_debug ("current_rib: k: %s, v: %d", kv.first.c_str (), kv.second);

  if (cursor ().parent.has_value ())
    for (const auto &kv : cursor ().parent.value ().rib.get_values ())
      rust_debug ("new cursor: k: %s, v: %d", kv.first.c_str (), kv.second);

  update_cursor (cursor ().parent.value ());
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
  return innermost_rib.insert (name.as_string (), node);
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
ForeverStack<N>::reverse_iter (std::function<KeepGoing (Rib &)> lambda)
{
  auto &tmp = cursor ();

  while (true)
    {
      auto keep_going = lambda (tmp);
      if (keep_going == KeepGoing::No)
	return;

      if (tmp.is_root ())
	return;

      tmp = tmp.parent.value ();
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
tl::optional<NodeId>
ForeverStack<N>::get (const Identifier &name)
{
  return {};
}

// TODO: Are there different fetching behavior for different namespaces?
// inline template <>
// tl::optional<NodeId>
// ForeverStack<Namespace::Values>::get (const Identifier &name)
// {
//   return {};
// }

template <Namespace N>
void
ForeverStack<N>::stream_rib (std::stringstream &stream, const Rib &rib,
			     const std::string &next,
			     const std::string &next_next)
{
  if (rib.get_values ().empty ())
    {
      stream << next << "rib: {},\n";
      return;
    }

  stream << next << "rib: {\n";

  for (const auto &kv : rib.get_values ())
    stream << next_next << kv.first << ": " << kv.second << "\n";

  stream << next << "},\n";
}

template <Namespace N>
void
ForeverStack<N>::stream_node (std::stringstream &stream, unsigned indentation,
			      const ForeverStack<N>::Node &node)
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
ForeverStack<N>::as_debug_string ()
{
  std::stringstream stream;

  stream_node (stream, 0, root);

  return stream.str ();
}

// FIXME: Can we add selftests?

} // namespace Resolver2_0
} // namespace Rust
