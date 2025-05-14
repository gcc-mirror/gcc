// Copyright (C) 2024 Free Software Foundation, Inc.

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
#include "rust-rib.h"
#include "optional.h"

namespace Rust {
namespace Resolver2_0 {

bool
ForeverStackStore::Node::is_root () const
{
  return !parent.has_value ();
}

bool
ForeverStackStore::Node::is_leaf () const
{
  return children.empty ();
}

NodeId
ForeverStackStore::Node::get_id () const
{
  return id;
}

ForeverStackStore::Node &
ForeverStackStore::Node::insert_child (NodeId id, tl::optional<Identifier> path,
				       Rib::Kind kind)
{
  auto res = children.insert ({Link (id, path), Node (kind, id, *this)});

  rust_debug ("inserting link: Link(%d [%s]): existed? %s", id,
	      path.has_value () ? path.value ().as_string ().c_str ()
				: "<anon>",
	      !res.second ? "yes" : "no");

  // sanity check on rib kind
  // pick the value rib, since all ribs should have the same kind anyways
  rust_assert (res.second || res.first->second.value_rib.kind == kind);

  // verify, if we're using an existing node, our paths don't contradict
  if (!res.second && path.has_value ())
    {
      auto other_path = res.first->first.path;
      rust_assert (!other_path.has_value ()
		   || other_path.value ().as_string ()
			== path.value ().as_string ());
    }

  return res.first->second;
}

tl::optional<ForeverStackStore::Node &>
ForeverStackStore::Node::get_child (const Identifier &path)
{
  for (auto &ent : children)
    {
      if (ent.first.path.has_value ()
	  && ent.first.path->as_string () == path.as_string ())
	return ent.second;
    }
  return tl::nullopt;
}

tl::optional<const ForeverStackStore::Node &>
ForeverStackStore::Node::get_child (const Identifier &path) const
{
  for (auto &ent : children)
    {
      if (ent.first.path.has_value ()
	  && ent.first.path->as_string () == path.as_string ())
	return ent.second;
    }
  return tl::nullopt;
}

tl::optional<ForeverStackStore::Node &>
ForeverStackStore::Node::get_parent ()
{
  return parent;
}

tl::optional<const ForeverStackStore::Node &>
ForeverStackStore::Node::get_parent () const
{
  if (parent)
    return *parent;
  return tl::nullopt;
}

tl::optional<const Identifier &>
ForeverStackStore::Node::get_parent_path () const
{
  if (parent.has_value ())
    for (auto &ent : parent->children)
      if (ent.first.id == id && ent.first.path.has_value ())
	return ent.first.path.value ();
  return tl::nullopt;
}

Rib &
ForeverStackStore::Node::get_rib (Namespace ns)
{
  switch (ns)
    {
    case Namespace::Values:
      return value_rib;
    case Namespace::Types:
      return type_rib;
    case Namespace::Labels:
      return label_rib;
    case Namespace::Macros:
      return macro_rib;
    default:
      rust_unreachable ();
    }
}

const Rib &
ForeverStackStore::Node::get_rib (Namespace ns) const
{
  switch (ns)
    {
    case Namespace::Values:
      return value_rib;
    case Namespace::Types:
      return type_rib;
    case Namespace::Labels:
      return label_rib;
    case Namespace::Macros:
      return macro_rib;
    default:
      rust_unreachable ();
    }
}

tl::expected<NodeId, DuplicateNameError>
ForeverStackStore::Node::insert (const Identifier &name, NodeId node,
				 Namespace ns)
{
  // So what do we do here - if the Rib has already been pushed in an earlier
  // pass, we might end up in a situation where it is okay to re-add new names.
  // Do we just ignore that here? Do we keep track of if the Rib is new or not?
  // should our cursor have info on the current node like "is it newly pushed"?
  return get_rib (ns).insert (name.as_string (),
			      Rib::Definition::NonShadowable (node));
}

tl::expected<NodeId, DuplicateNameError>
ForeverStackStore::Node::insert_shadowable (const Identifier &name, NodeId node,
					    Namespace ns)
{
  return get_rib (ns).insert (name.as_string (),
			      Rib::Definition::Shadowable (node));
}

tl::expected<NodeId, DuplicateNameError>
ForeverStackStore::Node::insert_globbed (const Identifier &name, NodeId node,
					 Namespace ns)
{
  return get_rib (ns).insert (name.as_string (),
			      Rib::Definition::Globbed (node));
}

void
ForeverStackStore::Node::reverse_iter (std::function<KeepGoing (Node &)> lambda)
{
  for (Node *tmp = this; lambda (*tmp) == KeepGoing::Yes && !tmp->is_root ();
       tmp = &tmp->parent.value ())
    ;
}

void
ForeverStackStore::Node::reverse_iter (
  std::function<KeepGoing (const Node &)> lambda) const
{
  for (const Node *tmp = this;
       lambda (*tmp) == KeepGoing::Yes && !tmp->is_root ();
       tmp = &tmp->parent.value ())
    ;
}

void
ForeverStackStore::Node::child_iter (
  std::function<KeepGoing (NodeId, tl::optional<const Identifier &>, Node &)>
    lambda)
{
  for (auto &ent : children)
    {
      tl::optional<const Identifier &> path;
      if (ent.first.path.has_value ())
	path = ent.first.path.value ();
      auto keep_going = lambda (ent.first.id, path, ent.second);
      if (keep_going == KeepGoing::No)
	return;
    }
}

void
ForeverStackStore::Node::child_iter (
  std::function<KeepGoing (NodeId, tl::optional<const Identifier &>,
			   const Node &)>
    lambda) const
{
  for (auto &ent : children)
    {
      tl::optional<const Identifier &> path;
      if (ent.first.path.has_value ())
	path = ent.first.path.value ();
      auto keep_going = lambda (ent.first.id, path, ent.second);
      if (keep_going == KeepGoing::No)
	return;
    }
}

ForeverStackStore::Node &
ForeverStackStore::Node::find_closest_module ()
{
  // get kind of value_rib
  // but all ribs should share the same kind anyways
  if (value_rib.kind == Rib::Kind::Module || !parent.has_value ())
    return *this;
  else
    return parent->find_closest_module ();
}

const ForeverStackStore::Node &
ForeverStackStore::Node::find_closest_module () const
{
  // get kind of value_rib
  // but all ribs should share the same kind anyways
  if (value_rib.kind != Rib::Kind::Module || !parent.has_value ())
    return *this;
  else
    return parent->find_closest_module ();
}

tl::optional<ForeverStackStore::Node &>
ForeverStackStore::Node::dfs_node (NodeId to_find)
{
  if (id == to_find)
    return *this;

  for (auto &child : children)
    {
      auto candidate = child.second.dfs_node (to_find);

      if (candidate.has_value ())
	return candidate;
    }

  return tl::nullopt;
}

tl::optional<const ForeverStackStore::Node &>
ForeverStackStore::Node::dfs_node (NodeId to_find) const
{
  if (id == to_find)
    return *this;

  for (auto &child : children)
    {
      auto candidate = child.second.dfs_node (to_find);

      if (candidate.has_value ())
	return candidate;
    }

  return tl::nullopt;
}

ForeverStackStore::Node &
ForeverStackStore::get_root ()
{
  return root;
}

const ForeverStackStore::Node &
ForeverStackStore::get_root () const
{
  return root;
}

tl::optional<ForeverStackStore::Node &>
ForeverStackStore::get_node (NodeId node_id)
{
  return root.dfs_node (node_id);
}

tl::optional<const ForeverStackStore::Node &>
ForeverStackStore::get_node (NodeId node_id) const
{
  return root.dfs_node (node_id);
}

} // namespace Resolver2_0
} // namespace Rust
