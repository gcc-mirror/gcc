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

#ifndef RUST_FOREVER_STACK_H
#define RUST_FOREVER_STACK_H

#include "rust-system.h"
#include "rust-rib.h"
#include "rust-ast.h"
#include "rust-path.h"
#include "optional.h"
#include "expected.h"

namespace Rust {
namespace Resolver2_0 {

/**

Let's look at our stack for resolving and traversing the following Rust code:

```rust
mod foo {
    mod bar {
	fn outer() {
	    fn inner() {}
	}

	fn another() {}
    }
}
```

We start by creating the stack, which contains only one rib - the crate's. We
won't look in details on how different namespaces end up with different stacks,
and will only consider the "value" namespace for this example. Modules do not
get added to the value namespace, but functions do:

```rust
let _ = foo;   // foo is a module, invalid Rust code
let _ = outer; // outer is a function, ok!
```

So passing each module will create a new Rib, but not add that module's node to
the Rib.

The current cursor of the stack will be denoted with `-->`: an arrow pointing to
the current rib.

When we start the `TopLevel` pass on the crate we are compiling, we only see the
top rib, which is empty at first:

      ┌───────────────┐
      │               │
  --> │               │
      │               │
      └───────────────┘

We pass through our first module, and emplace another Rib: Another "scope" is
created, and it impacts name resolution rules.

      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  foo │
	      │
	      ▼
      ┌───────────────┐
      │               │
  --> │               │
      │               │
      └───────────────┘

Notice that we have moved the cursor to the newly-created Rib, and that we have
added a path between the two ribs - this is a `Link`. A link contains
information such as the scope's NodeId, as well as an optional path - present
only when the scope is named. This allows us to easily fetch AST nodes based on
their canonical path, or build a canonical path from a NodeId. It also makes it
really easy to do complex path name resolution, such as `super::super::<item>`.
As mentioned earlier, modules are not present in the value namespace, so our new
rib is also empty. Let's pass through the second module:

      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  foo │
	      │
	      ▼
      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  bar │
	      │
	      ▼
      ┌───────────────┐
      │               │
  --> │               │
      │               │
      └───────────────┘

Once again, the new rib is empty, and we have a link with a path. We now go
through each item in the `bar` module and visit them. The first item is a
function, `outer` - upon being visited, it adds itself to the current rib.

      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  foo │
	      │
	      ▼
      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  bar │
	      │
	      ▼
      ┌───────────────┐
      │outer          │
  --> │               │
      │               │
      └───────────────┘

We now visit `outer`'s definition. This creates a new Rib, as functions can have
arguments, whose declaration only lives for the function's scope.

      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  foo │
	      │
	      ▼
      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  bar │
	      │
	      ▼
      ┌───────────────┐
      │outer          │
      │               │
      │               │
      └───────┬───────┘
	      │
       <anon> │
	      │
	      ▼
      ┌───────────────┐
      │               │
  --> │               │
      │               │
      └───────────────┘

This rib is anonymous (the link to it does not have a path), because we cannot
refer to a function's inner items from the outside:

```rust
pub mod a {
    pub fn foo() {}
}

pub fn b() {
    pub fn foo() {}
}

fn main() {
    a::foo(); // ok
    b::foo(); // ko!
}
```

We visit the function's block, which contain a single declaration, a function
named `inner`. It adds itself to the current rib.

      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  foo │
	      │
	      ▼
      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  bar │
	      │
	      ▼
      ┌───────────────┐
      │outer          │
      │               │
      │               │
      └───────┬───────┘
	      │
       <anon> │
	      │
	      ▼
      ┌───────────────┐
      │inner          │
  --> │               │
      │               │
      └───────────────┘

We visit `inner`, which yields a rib but no other declaration.

      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  foo │
	      │
	      ▼
      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  bar │
	      │
	      ▼
      ┌───────────────┐
      │outer          │
      │               │
      │               │
      └───────┬───────┘
	      │
       <anon> │
	      │
	      ▼
      ┌───────────────┐
      │inner          │
      │               │
      │               │
      └───────────────┘
	      │
       <anon> │
	      │
	      ▼
      ┌───────────────┐
      │               │
  --> │               │
      │               │
      └───────────────┘

We are now at the end of the `inner` function, and we want to pop the current
scope. Instead of deleting the current rib, we simply move the cursor backwards.
This allows us to keep track of the existing information and access it in later
name resolution passes. We then finish visiting `outer`, then go back to our
`bar` module. This is what our stack looks like after this. Note how the only
difference is the cursor's location.

      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  foo │
	      │
	      ▼
      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  bar │
	      │
	      ▼
      ┌───────────────┐
      │outer          │
  --> │               │
      │               │
      └───────┬───────┘
	      │
       <anon> │
	      │
	      ▼
      ┌───────────────┐
      │inner          │
      │               │
      │               │
      └───────────────┘
	      │
       <anon> │
	      │
	      ▼
      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────────────┘

We then visit the remaining `bar` items, which are composed of the `another`
function. It adds itself to the current rib. This function contains no
declarations, but it still creates a Rib upon being visited. We then finish our
visit of `bar`, which marks the end of our visit of `foo`, which marks the end
of our `TopLevel` name resolution pass.

      ┌───────────────┐
      │               │
  --> │               │
      │               │
      └───────┬───────┘
	      │
	  foo │
	      │
	      ▼
      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────┬───────┘
	      │
	  bar │
	      │
	      ▼
      ┌───────────────┐
      │outer          │
      │another        │
      │               │
      └───────┬──┬────┘
	      │  │       <anon>
       <anon> │  └────────────────────┐
	      │                       │
	      ▼                       ▼
      ┌───────────────┐       ┌───────────────┐
      │inner          │       │               │
      │               │       │               │
      │               │       │               │
      └───────┬───────┘       └───────────────┘
	      │
       <anon> │
	      │
	      ▼
      ┌───────────────┐
      │               │
      │               │
      │               │
      └───────────────┘

We now have a stack with a lot of ribs, prime for the `Early` and `Late` name
resolution passes. We will revisit the ribs we created in these passes, and we
won't need to allocate or create new ones: because they will still be present in
the stack, we will simply move our cursor to these ribs. In this case, there is
nothing to do, since there are no uses of our definitions, as the Rust code we
are name-resolving is not really interesting. You'll also note that our
`TopLevel` pass did not resolve a whole lot: all it did was create new ribs, and
empty ones at that. The `Early` pass will not go further, since our code does
not contain any imports, macro definitions or macro invocations. You can look at
this pass's documentation for more details on this resolution process.

**/

/**
 * Intended for use by ForeverStack to store Nodes
 * Unlike ForeverStack, does not store a cursor reference
 * Intended to make path resolution in multiple namespaces simpler
 **/
class ForeverStackStore
{
public:
  ForeverStackStore (NodeId crate_id) : root (Rib::Kind::Normal, crate_id)
  {
    rust_assert (root.is_root ());
    rust_assert (root.is_leaf ());
  }

private:
  /**
   * A link between two Nodes in our trie data structure. This class represents
   * the edges of the graph
   */
  class Link
  {
  public:
    Link (NodeId id, tl::optional<Identifier> path) : id (id), path (path) {}

    bool compare (const Link &other) const { return id < other.id; }

    NodeId id;
    tl::optional<Identifier> path;
  };

  /* Link comparison class, which we use in a Node's `children` map */
  class LinkCmp
  {
  public:
    bool operator() (const Link &lhs, const Link &rhs) const
    {
      return lhs.compare (rhs);
    }
  };

public:
  class Node;

  struct DfsResult
  {
    Node &first;
    std::string second;
  };

  struct ConstDfsResult
  {
    const Node &first;
    std::string second;
  };

  /* Should we keep going upon seeing a Rib? */
  enum class KeepGoing
  {
    Yes,
    No,
  };

  class Node
  {
  private:
    friend class ForeverStackStore::ForeverStackStore;

    Node (Rib::Kind rib_kind, NodeId id, tl::optional<Node &> parent)
      : value_rib (rib_kind), type_rib (rib_kind), label_rib (rib_kind),
	macro_rib (rib_kind), id (id), parent (parent)
    {}
    Node (Rib::Kind rib_kind, NodeId id) : Node (rib_kind, id, tl::nullopt) {}
    Node (Rib::Kind rib_kind, NodeId id, Node &parent)
      : Node (rib_kind, id, tl::optional<Node &> (parent))
    {}

  public:
    Node (const Node &) = default;
    Node (Node &&) = default;
    Node &operator= (const Node &) = delete;
    Node &operator= (Node &&) = default;

    bool is_root () const;
    bool is_leaf () const;

    NodeId get_id () const;

    Node &insert_child (NodeId id, tl::optional<Identifier> path,
			Rib::Kind kind);

    tl::optional<Node &> get_child (const Identifier &path);
    tl::optional<const Node &> get_child (const Identifier &path) const;

    tl::optional<Node &> get_parent ();
    tl::optional<const Node &> get_parent () const;

    // finds the identifier, if any, used to link
    // this node's parent to this node
    tl::optional<const Identifier &> get_parent_path () const;

    Rib &get_rib (Namespace ns);
    const Rib &get_rib (Namespace ns) const;

    tl::expected<NodeId, DuplicateNameError> insert (const Identifier &name,
						     NodeId node, Namespace ns);
    tl::expected<NodeId, DuplicateNameError>
    insert_shadowable (const Identifier &name, NodeId node, Namespace ns);
    tl::expected<NodeId, DuplicateNameError>
    insert_globbed (const Identifier &name, NodeId node, Namespace ns);

    void reverse_iter (std::function<KeepGoing (Node &)> lambda);
    void reverse_iter (std::function<KeepGoing (const Node &)> lambda) const;

    void child_iter (std::function<KeepGoing (
		       NodeId, tl::optional<const Identifier &>, Node &)>
		       lambda);
    void child_iter (std::function<KeepGoing (
		       NodeId, tl::optional<const Identifier &>, const Node &)>
		       lambda) const;

    Node &find_closest_module ();
    const Node &find_closest_module () const;

    tl::optional<Node &> dfs_node (NodeId to_find);
    tl::optional<const Node &> dfs_node (NodeId to_find) const;

  private:
    // per-namespace ribs
    Rib value_rib;
    Rib type_rib;
    Rib label_rib;
    Rib macro_rib;
    // all linked nodes
    std::map<Link, Node, LinkCmp> children;

    NodeId id; // The node id of the Node's scope

    tl::optional<Node &> parent; // `None` only if the node is a root
  };

  Node &get_root ();
  const Node &get_root () const;

  tl::optional<Node &> get_node (NodeId node_id);
  tl::optional<const Node &> get_node (NodeId node_id) const;

private:
  Node root;
};

template <Namespace N> class ForeverStack
{
public:
  ForeverStack ()
    : root (Node (Rib (Rib::Kind::Normal), UNKNOWN_NODEID)),
      lang_prelude (Node (Rib (Rib::Kind::Prelude), UNKNOWN_NODEID, root)),
      extern_prelude (Node (Rib (Rib::Kind::Prelude), UNKNOWN_NODEID)),
      cursor_reference (root)
  {
    rust_assert (root.is_root ());
    rust_assert (root.is_leaf ());

    // TODO: Should we be using the forever stack root as the crate scope?
    // TODO: Is this how we should be getting the crate node id?
    auto &mappings = Analysis::Mappings::get ();
    root.id = *mappings.crate_num_to_nodeid (mappings.get_current_crate ());
  }

  /**
   * Add a new Rib to the stack. If the Rib already exists, nothing is pushed
   * and the stack's cursor is simply moved to this existing Rib.
   *
   * @param rib The Rib to push
   * @param id The NodeId of the node for which the Rib was created. For
   *        example, if a Rib is created because a lexical scope is entered,
   *        then `id` is that `BlockExpr`'s NodeId.
   * @param path An optional path if the Rib was created due to a "named"
   *        lexical scope, like a module's.
   */
  void push (Rib::Kind rib_kind, NodeId id, tl::optional<Identifier> path = {});

  /**
   * Pop the innermost Rib from the stack
   */
  void pop ();

  /**
   * Insert a new definition in the innermost `Rib` in this stack
   *
   * @param name The name of the definition
   * @param id Its NodeId
   *
   * @return `DuplicateNameError` if that node was already present in the Rib,
   * the node's `NodeId` otherwise.
   *
   * @aborts if there are no `Rib`s inserted in the current map, this function
   *         aborts the program.
   */
  tl::expected<NodeId, DuplicateNameError> insert (Identifier name, NodeId id);

  tl::expected<NodeId, DuplicateNameError> insert_variant (Identifier name,
							   NodeId id);

  /**
   * Insert a new shadowable definition in the innermost `Rib` in this stack
   *
   * @param name The name of the definition
   * @param id Its NodeId
   *
   * @return `DuplicateNameError` if that node was already present in the Rib,
   * the node's `NodeId` otherwise.
   *
   * @aborts if there are no `Rib`s inserted in the current map, this function
   *         aborts the program.
   */
  tl::expected<NodeId, DuplicateNameError> insert_shadowable (Identifier name,
							      NodeId id);

  /**
   * Insert a new glob-originated definition in the innermost `Rib` in this
   * stack
   *
   * @param name The name of the definition
   * @param id Its NodeId
   *
   * @return `DuplicateNameError` if that node was already present in the Rib,
   * the node's `NodeId` otherwise.
   *
   * @aborts if there are no `Rib`s inserted in the current map, this function
   *         aborts the program.
   */
  tl::expected<NodeId, DuplicateNameError> insert_globbed (Identifier name,
							   NodeId id);

  /**
   * Insert a new definition at the root of this stack
   *
   * @param name The name of the definition
   * @param id Its NodeId
   *
   * @return `DuplicateNameError` if that node was already present in the Rib,
   * the node's `NodeId` otherwise.
   *
   * @aborts if there are no `Rib`s inserted in the current map, this function
   *         aborts the program.
   */
  tl::expected<NodeId, DuplicateNameError> insert_at_root (Identifier name,
							   NodeId id);

  /* Access the innermost `Rib` in this map */
  Rib &peek ();
  const Rib &peek () const;

  /**
   * Reverse iter on all ribs from the innermost one to the outermost one,
   * trying to find a name. This is the default algorithm.
   * This function gets specialized based on the Rib::Kind
   * this way, we ensure a proper resolution algorithm at the type level
   *
   * @param name Name of the identifier to locate in this scope or an outermore
   *        scope
   *
   * @return a valid option with the Definition if the identifier is present in
   * the current map, an empty one otherwise.
   */
  tl::optional<Rib::Definition> get (const Identifier &name);
  tl::optional<Rib::Definition> get_lang_prelude (const Identifier &name);
  tl::optional<Rib::Definition> get_lang_prelude (const std::string &name);

  /**
   * Resolve a path to its definition in the current `ForeverStack`
   *
   * // TODO: Add documentation for `segments`
   *
   * @return a valid option with the Definition if the path is present in the
   *         current map, an empty one otherwise.
   */
  template <typename S>
  tl::optional<Rib::Definition> resolve_path (
    const std::vector<S> &segments, bool has_opening_scope_resolution,
    std::function<void (const S &, NodeId)> insert_segment_resolution);

  // FIXME: Documentation
  tl::optional<Resolver::CanonicalPath> to_canonical_path (NodeId id) const;

  // FIXME: Documentation
  tl::optional<Rib &> to_rib (NodeId rib_id);
  tl::optional<const Rib &> to_rib (NodeId rib_id) const;

  std::string as_debug_string () const;

  /**
   * Used to check if a module is a descendant of another module
   * Intended for use in the privacy checker
   */
  bool is_module_descendant (NodeId parent, NodeId child) const;

private:
  /**
   * A link between two Nodes in our trie data structure. This class represents
   * the edges of the graph
   */
  class Link
  {
  public:
    Link (NodeId id, tl::optional<Identifier> path) : id (id), path (path) {}

    bool compare (const Link &other) const { return id < other.id; }

    NodeId id;
    tl::optional<Identifier> path;
  };

  /* Link comparison class, which we use in a Node's `children` map */
  class LinkCmp
  {
  public:
    bool operator() (const Link &lhs, const Link &rhs) const
    {
      return lhs.compare (rhs);
    }
  };

  class Node
  {
  public:
    Node (Rib rib, NodeId id) : rib (rib), id (id) {}
    Node (Rib rib, NodeId id, Node &parent)
      : rib (rib), id (id), parent (parent)
    {}

    bool is_root () const;
    bool is_prelude () const;
    bool is_leaf () const;

    void insert_child (Link link, Node child);

    Rib rib; // this is the "value" of the node - the data it keeps.
    std::map<Link, Node, LinkCmp> children; // all the other nodes it links to

    NodeId id; // The node id of the Node's scope

    tl::optional<Node &> parent; // `None` only if the node is a root
  };

  /* Should we keep going upon seeing a Rib? */
  enum class KeepGoing
  {
    Yes,
    No,
  };

  /* Add a new Rib to the stack. This is an internal method */
  void push_inner (Rib rib, Link link);

  /* Reverse iterate on `Node`s from the cursor, in an outwards fashion */
  void reverse_iter (std::function<KeepGoing (Node &)> lambda);
  void reverse_iter (std::function<KeepGoing (const Node &)> lambda) const;

  /* Reverse iterate on `Node`s from a specified one, in an outwards fashion */
  void reverse_iter (Node &start, std::function<KeepGoing (Node &)> lambda);
  void reverse_iter (const Node &start,
		     std::function<KeepGoing (const Node &)> lambda) const;

  Node &cursor ();
  const Node &cursor () const;
  void update_cursor (Node &new_cursor);

  /* The forever stack's actual nodes */
  Node root;
  /*
   * A special prelude node used currently for resolving language builtins
   * It has the root node as a parent, and acts as a "special case" for name
   * resolution
   */
  Node lang_prelude;
  /*
   * The extern prelude, used for resolving external crates
   */
  Node extern_prelude;

  std::reference_wrapper<Node> cursor_reference;

  void stream_rib (std::stringstream &stream, const Rib &rib,
		   const std::string &next, const std::string &next_next) const;
  void stream_node (std::stringstream &stream, unsigned indentation,
		    const Node &node) const;

  /* Helper types and functions for `resolve_path` */

  template <typename S>
  using SegIterator = typename std::vector<S>::const_iterator;

  Node &find_closest_module (Node &starting_point);

  template <typename S>
  tl::optional<SegIterator<S>> find_starting_point (
    const std::vector<S> &segments,
    std::reference_wrapper<Node> &starting_point,
    std::function<void (const S &, NodeId)> insert_segment_resolution);

  template <typename S>
  tl::optional<Node &> resolve_segments (
    Node &starting_point, const std::vector<S> &segments,
    SegIterator<S> iterator,
    std::function<void (const S &, NodeId)> insert_segment_resolution);

  tl::optional<Rib::Definition> resolve_final_segment (Node &final_node,
						       std::string &seg_name,
						       bool is_lower_self);

  /* Helper functions for forward resolution (to_canonical_path, to_rib...) */
  struct DfsResult
  {
    Node &first;
    std::string second;
  };
  struct ConstDfsResult
  {
    const Node &first;
    std::string second;
  };

  // FIXME: Documentation
  tl::optional<DfsResult> dfs (Node &starting_point, NodeId to_find);
  tl::optional<ConstDfsResult> dfs (const Node &starting_point,
				    NodeId to_find) const;
  // FIXME: Documentation
  tl::optional<Rib &> dfs_rib (Node &starting_point, NodeId to_find);
  tl::optional<const Rib &> dfs_rib (const Node &starting_point,
				     NodeId to_find) const;
  // FIXME: Documentation
  tl::optional<Node &> dfs_node (Node &starting_point, NodeId to_find);
  tl::optional<const Node &> dfs_node (const Node &starting_point,
				       NodeId to_find) const;
};

} // namespace Resolver2_0
} // namespace Rust

#include "rust-forever-stack.hxx"

#endif // !RUST_FOREVER_STACK_H
