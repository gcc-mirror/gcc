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

#ifndef RUST_NAME_RESOLVER_2_0_H
#define RUST_NAME_RESOLVER_2_0_H

#include "optional.h"
#include "rust-forever-stack.h"
#include "rust-hir-map.h"

namespace Rust {
namespace Resolver2_0 {

// TODO: Add missing mappings and data structures

/**
The data structures we need to develop need to fill in a few roles - like the
original name resolver, they need to be accessible at multiple points during the
pipeline to allow compiler passes such as macro expansion or typechecking to
benefit from them. Unlike the original name resolution, these data structures
need to be created by multiple compiler passes: Whereas the original name
resolution of gccrs tries to perform name resolution in a single pass, it fails
at properly handling more complex name resolution cases such as macro name
resolution, imports in general, and glob imports in particular. The goal of this
new name resolution algorithm is to split the name resolution in at least two
passes - `Early` name resolution, which takes care of macro name resolution and
import resolution, and `Late` name resolution - your typical name resolution,
for types, functions, variables...

  1. `Early`

  The Early name resolution is tied in snuggly with macro expansion: macro
expansion cannot happen without some form of name resolution (pointing an
invocation to its definition) but may also *depend* on name resolution (a macro
generating another macro... or importing items... and funny other cases like
these). It needs to work in a fixed-point fashion alongside macro expansion:
While there are imports to resolve, or macros to expand, we need to keep going
and resolve them. This is achieved, among other things, by a top-level name
resolution pass in charge of collection use statements and macro definitions (as
well as Items, which will be useful for later passes of the name resolution).

    This top-level pass exists because Rust enables you to call a function
before having declared it (at a lexical level, i.e calling `f(15)` at line 3
while the `f` function is declared at line 1499).

  This Early pass needs to build the first part of our "resolution map", which
will then be used in multiple contexts:

  1. The MacroExpander, in a read-only fashion: fetching macro definitions for
each invocation and performing the expansion.
  2. `Late`, which will write more data inside that resolution map, and use it
to perform its name resolution too.

  This is where the first challenge of this data structure lies: The existing
data structures and name resolution algorithm relies on the name resolution pass
happening just once. In typical name resolution fashion, when it sees a lexical
scope (a new module, a function's block, a block expression...), it "pushes" a
new "Scope" to a stack of these scopes, and "pops" it when exiting said lexical
scope. However, because we are splitting the name resolution into two passes, we
would like to avoid re-doing a bunch of work we've already done - which is why
this data structure needs to allow "re-entrancy", or to at least not keep as
much state as the existing one, and allow for viewing the same module multiple
times without throwing a fit.

  We will be implementing a "forever stack" of scopes, which allows the user the
pushing of new scopes onto the stack, but only simulates the popping of a scope:
When pushing new scopes, more space is allocated on our stack, and we keep
track of this scope as being the current one - however, when popping this scope,
we do not actually delete the memory associated with it: we simply mark the
previous scope (parent) as the current one.

In the example below, each number indicates the "state" of our resolution map,
and the carret is used to point to the current lexical scope.

```rust
		// []
		//
fn main() {     // [ `main` scope: {} ]
		//         ^
  let a = 15;   // [ `main` scope: { Decl(a) } ]
		//         ^
  {  _PUSH_     // [ `main` scope: { Decl(a) }, anonymous scope: {} ]
		//                                        ^
    let a = 16; // [ `main` scope: { Decl(a) }, anonymous scope: { Decl(a) } ]
		//                                        ^
    f(a);       // [ `main` scope: { Decl(a) }, anonymous scope: { Decl(a) } ]
		//                                        ^
  }   _POP_     // [ `main` scope: { Decl(a) }, anonymous scope: { Decl(a) } ]
		//         ^
  f(a);         // [ `main` scope: { Decl(a) }, anonymous scope: { Decl(a) } ]
		//         ^
}
```

This allows us to revisit scopes previously visited in later phases of the name
resolution, and add more information if necessary.

  2. `Late`

  `Late` name resolution possesses some unique challenges since Rust's name
resolution rules are extremely complex - variable shadowing, variable capture in
closures (but not inner functions!)... You can have a look at a fucked up
example here:

https://rustc-dev-guide.rust-lang.org/name-resolution.html#scopes-and-ribs

This requires us to think about what exactly to put in our `Scope`s and what to
do with our `Rib`s - and how it affects our data structures. For example, in the
above example, `rustc` demonstrates how multiple `Rib`s can be created inside of
a single lexical scope for variables, as the Rust programming language allows
shadowing.

    TODO: Mention macro hygiene and that it is the same
    TODO: How does this affect our data structures?
    TODO: Last challenge - reuse the same APIs to allow the typechecker to not
change?
    TODO: Mention that ForeverStack is templated to make sure that behavior is
correct
*/

// FIXME: Documentation
class Usage
{
public:
  explicit Usage (NodeId id) : id (id) {}

  // TODO: move to name-resolution-ctx.cc
  // storing it as a key in a map
  bool operator< (const Usage other) const { return other.id < id; }

  NodeId id;
};

// FIXME: Documentation
class Definition
{
public:
  explicit Definition (NodeId id) : id (id) {}

  NodeId id;
};

// Now our resolver, which keeps track of all the `ForeverStack`s we could want
class NameResolutionContext
{
public:
  NameResolutionContext ();

  /**
   * Insert a new value in the current rib.
   *
   * @param name Name of the value to insert.
   * @param id This value's ID, e.g the function definition's node ID.
   * @param ns Namespace in which to insert the value.
   */
  tl::expected<NodeId, DuplicateNameError> insert (Identifier name, NodeId id,
						   Namespace ns);

  tl::expected<NodeId, DuplicateNameError>
  insert_shadowable (Identifier name, NodeId id, Namespace ns);

  /**
   * Run a lambda in a "scoped" context, meaning that a new `Rib` will be pushed
   * before executing the lambda and then popped. This is useful for all kinds
   * of scope in the language, such as a block expression or when entering a
   * function. This variant of the function enters a new scope in *all*
   * namespaces, while the second variant enters a scope in *one* namespace.
   *
   * @param rib New `Rib` to create when entering this scope. A function `Rib`,
   *        or an item `Rib`... etc
   * @param scope_id node ID of the scope we are entering, e.g the block's
   *        `NodeId`.
   * @param lambda Function to run within that scope
   * @param path Optional path of the scope. This is useful for scopes which
   *        affect path resolution, such as modules. Defaults to an empty
   *        option.
   */
  // FIXME: Do we want to handle something in particular for expected within the
  // scoped lambda?
  void scoped (Rib rib, NodeId scope_id, std::function<void (void)> lambda,
	       tl::optional<Identifier> path = {});
  void scoped (Rib rib, Namespace ns, NodeId scope_id,
	       std::function<void (void)> lambda,
	       tl::optional<Identifier> path = {});

  ForeverStack<Namespace::Values> values;
  ForeverStack<Namespace::Types> types;
  ForeverStack<Namespace::Macros> macros;
  ForeverStack<Namespace::Labels> labels;

  Analysis::Mappings &mappings;

  // TODO: Rename
  // TODO: Use newtype pattern for Usage and Definition
  void map_usage (Usage usage, Definition definition);

  tl::optional<NodeId> lookup (NodeId usage);

private:
  /* Map of "usage" nodes which have been resolved to a "definition" node */
  std::map<Usage, Definition> resolved_nodes;
};

} // namespace Resolver2_0
} // namespace Rust

#endif // ! RUST_NAME_RESOLVER_2_0_H
