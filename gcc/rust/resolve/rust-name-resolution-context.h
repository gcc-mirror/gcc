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
#include "rust-rib.h"
#include "rust-stacked-contexts.h"
#include "rust-item.h"

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

struct Binding
{
  enum class Kind
  {
    Product,
    Or,
  } kind;

  std::unordered_set<Identifier> set;

  Binding (Binding::Kind kind) : kind (kind) {}
};

/**
 * Used to identify the source of a binding, and emit the correct error message.
 */
enum class BindingSource
{
  Match,
  Let,
  IfLet,
  For,
  /* Closure param or function param */
  Param
};

class BindingLayer
{
  BindingSource source;
  std::vector<Binding> bindings;

  bool bind_test (Identifier ident, Binding::Kind kind);

public:
  void push (Binding::Kind kind);

  BindingLayer (BindingSource source);

  /**
   * Identifies if the identifier has been used in a product binding context.
   * eg. `let (a, a) = test();`
   */
  bool is_and_bound (Identifier ident);

  /**
   * Identifies if the identifier has been used in a or context.
   * eg. `let (a, 1) | (a, 2) = test()`
   */
  bool is_or_bound (Identifier ident);

  void insert_ident (Identifier ident);

  void merge ();

  BindingSource get_source () const;
};

class NameResolutionContext;
/*
 * Used to handle canonical paths
 * Similar to ForeverStack, but namespace independent and more specialized
 */
class CanonicalPathRecord
{
public:
  virtual Resolver::CanonicalPath as_path (const NameResolutionContext &) = 0;

  virtual bool is_root () const = 0;

  virtual ~CanonicalPathRecord () = default;
};

class CanonicalPathRecordWithParent : public CanonicalPathRecord
{
public:
  CanonicalPathRecordWithParent (CanonicalPathRecord &parent) : parent (&parent)
  {}

  CanonicalPathRecord &get_parent () { return *parent; }

  bool is_root () const override final { return false; }

private:
  CanonicalPathRecord *parent;
};

class CanonicalPathRecordCrateRoot : public CanonicalPathRecord
{
public:
  CanonicalPathRecordCrateRoot (NodeId node_id, std::string seg)
    : node_id (node_id), seg (std::move (seg))
  {
    rust_assert (Analysis::Mappings::get ().node_is_crate (node_id));
    crate_num = Analysis::Mappings::get ().lookup_crate_num (node_id).value ();
  }

  Resolver::CanonicalPath as_path (const NameResolutionContext &) override;

  bool is_root () const override final { return true; }

private:
  NodeId node_id;
  CrateNum crate_num;
  std::string seg;
};

class CanonicalPathRecordNormal : public CanonicalPathRecordWithParent
{
public:
  CanonicalPathRecordNormal (CanonicalPathRecord &parent, NodeId node_id,
			     std::string seg)
    : CanonicalPathRecordWithParent (parent), node_id (node_id),
      seg (std::move (seg))
  {
    rust_assert (!Analysis::Mappings::get ().node_is_crate (node_id));
  }

  Resolver::CanonicalPath as_path (const NameResolutionContext &) override;

private:
  NodeId node_id;
  std::string seg;
};

class CanonicalPathRecordLookup : public CanonicalPathRecord
{
public:
  CanonicalPathRecordLookup (NodeId lookup_id)
    : lookup_id (lookup_id), cache (nullptr)
  {}

  Resolver::CanonicalPath as_path (const NameResolutionContext &) override;

  bool is_root () const override final { return true; }

private:
  NodeId lookup_id;
  CanonicalPathRecord *cache;
};

class CanonicalPathRecordImpl : public CanonicalPathRecordWithParent
{
public:
  CanonicalPathRecordImpl (CanonicalPathRecord &parent, NodeId impl_id,
			   NodeId type_id)
    : CanonicalPathRecordWithParent (parent), impl_id (impl_id),
      type_record (type_id)
  {}

  Resolver::CanonicalPath as_path (const NameResolutionContext &) override;

private:
  NodeId impl_id;
  CanonicalPathRecordLookup type_record;
};

class CanonicalPathRecordTraitImpl : public CanonicalPathRecordWithParent
{
public:
  CanonicalPathRecordTraitImpl (CanonicalPathRecord &parent, NodeId impl_id,
				NodeId type_id, NodeId trait_path_id)
    : CanonicalPathRecordWithParent (parent), impl_id (impl_id),
      type_record (type_id), trait_path_record (trait_path_id)
  {}

  Resolver::CanonicalPath as_path (const NameResolutionContext &) override;

private:
  NodeId impl_id;
  CanonicalPathRecordLookup type_record;
  CanonicalPathRecordLookup trait_path_record;
};

class CanonicalPathCtx
{
public:
  CanonicalPathCtx (const NameResolutionContext &ctx)
    : current_record (nullptr), nr_ctx (&ctx)
  {}

  Resolver::CanonicalPath get_path (NodeId id) const
  {
    return get_record (id).as_path (*nr_ctx);
  }

  CanonicalPathRecord &get_record (NodeId id) const
  {
    auto it = records.find (id);
    rust_assert (it != records.end ());
    return *it->second;
  }

  tl::optional<CanonicalPathRecord *> get_record_opt (NodeId id) const
  {
    auto it = records.find (id);
    if (it == records.end ())
      return tl::nullopt;
    else
      return it->second.get ();
  }

  void insert_record (NodeId id, const Identifier &ident)
  {
    insert_record (id, ident.as_string ());
  }

  void insert_record (NodeId id, std::string seg)
  {
    rust_assert (current_record != nullptr);

    auto it = records.find (id);
    if (it == records.end ())
      {
	auto record = new CanonicalPathRecordNormal (*current_record, id,
						     std::move (seg));
	bool ok
	  = records.emplace (id, std::unique_ptr<CanonicalPathRecord> (record))
	      .second;
	rust_assert (ok);
      }
  }

  template <typename F> void scope (NodeId id, const Identifier &ident, F &&f)
  {
    scope (id, ident.as_string (), std::forward<F> (f));
  }

  template <typename F> void scope (NodeId id, std::string seg, F &&f)
  {
    rust_assert (current_record != nullptr);

    scope_inner (id, std::forward<F> (f), [this, id, &seg] () {
      return new CanonicalPathRecordNormal (*current_record, id,
					    std::move (seg));
    });
  }

  template <typename F> void scope_impl (AST::InherentImpl &impl, F &&f)
  {
    rust_assert (current_record != nullptr);

    NodeId id = impl.get_node_id ();
    scope_inner (id, std::forward<F> (f), [this, id, &impl] () {
      return new CanonicalPathRecordImpl (*current_record, id,
					  impl.get_type ().get_node_id ());
    });
  }

  template <typename F> void scope_impl (AST::TraitImpl &impl, F &&f)
  {
    rust_assert (current_record != nullptr);

    NodeId id = impl.get_node_id ();
    scope_inner (id, std::forward<F> (f), [this, id, &impl] () {
      return new CanonicalPathRecordTraitImpl (
	*current_record, id, impl.get_type ().get_node_id (),
	impl.get_trait_path ().get_node_id ());
    });
  }

  template <typename F>
  void scope_crate (NodeId node_id, std::string crate_name, F &&f)
  {
    scope_inner (node_id, std::forward<F> (f), [node_id, &crate_name] () {
      return new CanonicalPathRecordCrateRoot (node_id, std::move (crate_name));
    });
  }

private:
  template <typename FCreate, typename FCallback>
  void scope_inner (NodeId id, FCallback &&f_callback, FCreate &&f_create)
  {
    auto it = records.find (id);
    if (it == records.end ())
      {
	CanonicalPathRecord *record = std::forward<FCreate> (f_create) ();
	it = records.emplace (id, std::unique_ptr<CanonicalPathRecord> (record))
	       .first;
      }

    rust_assert (it->second->is_root ()
		 || &static_cast<CanonicalPathRecordWithParent &> (*it->second)
			.get_parent ()
		      == current_record);

    CanonicalPathRecord *stash = it->second.get ();
    std::swap (stash, current_record);

    std::forward<FCallback> (f_callback) ();

    std::swap (stash, current_record);
  }

  std::unordered_map<NodeId, std::unique_ptr<CanonicalPathRecord>> records;
  CanonicalPathRecord *current_record;

  const NameResolutionContext *nr_ctx;
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

  tl::expected<NodeId, DuplicateNameError> insert_variant (Identifier name,
							   NodeId id);

  tl::expected<NodeId, DuplicateNameError>
  insert_shadowable (Identifier name, NodeId id, Namespace ns);

  tl::expected<NodeId, DuplicateNameError>
  insert_globbed (Identifier name, NodeId id, Namespace ns);

  /**
   * Run a lambda in a "scoped" context, meaning that a new `Rib` will be pushed
   * before executing the lambda and then popped. This is useful for all kinds
   * of scope in the language, such as a block expression or when entering a
   * function. This variant of the function enters a new scope in *all*
   * namespaces, while the second variant enters a scope in *one* namespace.
   *
   * @param rib_kind New `Rib` to create when entering this scope. A function
   *        `Rib`, or an item `Rib`... etc
   * @param scope_id node ID of the scope we are entering, e.g the block's
   *        `NodeId`.
   * @param lambda Function to run within that scope
   * @param path Optional path of the scope. This is useful for scopes which
   *        affect path resolution, such as modules. Defaults to an empty
   *        option.
   */
  // FIXME: Do we want to handle something in particular for expected within the
  // scoped lambda?
  void scoped (Rib::Kind rib_kind, NodeId scope_id,
	       std::function<void (void)> lambda,
	       tl::optional<Identifier> path = {});
  void scoped (Rib::Kind rib_kind, Namespace ns, NodeId scope_id,
	       std::function<void (void)> lambda,
	       tl::optional<Identifier> path = {});

  ForeverStack<Namespace::Values> values;
  ForeverStack<Namespace::Types> types;
  ForeverStack<Namespace::Macros> macros;
  ForeverStack<Namespace::Labels> labels;

  Analysis::Mappings &mappings;
  StackedContexts<BindingLayer> bindings;

  CanonicalPathCtx canonical_ctx;

  // TODO: Rename
  // TODO: Use newtype pattern for Usage and Definition
  void map_usage (Usage usage, Definition definition);

  tl::optional<NodeId> lookup (NodeId usage) const;

  Resolver::CanonicalPath to_canonical_path (NodeId id) const
  {
    return canonical_ctx.get_path (id);
  }

  template <typename S>
  tl::optional<Rib::Definition>
  resolve_path (const std::vector<S> &segments, ResolutionMode mode,
		std::vector<Error> &collect_errors, Namespace ns)
  {
    std::function<void (const S &, NodeId)> insert_segment_resolution
      = [this] (const S &seg, NodeId id) {
	  auto seg_id = unwrap_segment_node_id (seg);
	  if (resolved_nodes.find (Usage (seg_id)) == resolved_nodes.end ())
	    map_usage (Usage (seg_id), Definition (id));
	};
    switch (ns)
      {
      case Namespace::Values:
	return values.resolve_path (segments, mode, insert_segment_resolution,
				    collect_errors);
      case Namespace::Types:
	return types.resolve_path (segments, mode, insert_segment_resolution,
				   collect_errors);
      case Namespace::Macros:
	return macros.resolve_path (segments, mode, insert_segment_resolution,
				    collect_errors);
      case Namespace::Labels:
	return labels.resolve_path (segments, mode, insert_segment_resolution,
				    collect_errors);
      default:
	rust_unreachable ();
      }
  }

  template <typename S, typename... Args>
  tl::optional<Rib::Definition>
  resolve_path (const std::vector<S> &segments, ResolutionMode mode,
		tl::optional<std::vector<Error> &> collect_errors,
		Namespace ns_first, Args... ns_args)
  {
    std::initializer_list<Namespace> namespaces = {ns_first, ns_args...};

    for (auto ns : namespaces)
      {
	std::vector<Error> collect_errors_inner;
	if (auto ret = resolve_path (segments, mode, collect_errors_inner, ns))
	  return ret;
	if (!collect_errors_inner.empty ())
	  {
	    if (collect_errors.has_value ())
	      {
		std::move (collect_errors_inner.begin (),
			   collect_errors_inner.end (),
			   std::back_inserter (collect_errors.value ()));
	      }
	    else
	      {
		for (auto &e : collect_errors_inner)
		  e.emit ();
	      }
	    return tl::nullopt;
	  }
      }

    return tl::nullopt;
  }

  template <typename S, typename... Args>
  tl::optional<Rib::Definition>
  resolve_path (const std::vector<S> &path_segments,
		bool has_opening_scope_resolution,
		tl::optional<std::vector<Error> &> collect_errors,
		Namespace ns_first, Args... ns_args)
  {
    auto mode = ResolutionMode::Normal;
    if (has_opening_scope_resolution)
      {
	if (get_rust_edition () == Edition::E2015)
	  mode = ResolutionMode::FromRoot;
	else
	  mode = ResolutionMode::FromExtern;
      }
    return resolve_path (path_segments, mode, collect_errors, ns_first,
			 ns_args...);
  }

  template <typename S, typename... Args>
  tl::optional<Rib::Definition>
  resolve_path (const std::vector<S> &path_segments,
		bool has_opening_scope_resolution, Namespace ns_first,
		Args... ns_args)
  {
    return resolve_path (path_segments, has_opening_scope_resolution,
			 tl::nullopt, ns_first, ns_args...);
  }

  template <typename S, typename... Args>
  tl::optional<Rib::Definition>
  resolve_path (const std::vector<S> &path_segments, ResolutionMode mode,
		Namespace ns_first, Args... ns_args)
  {
    return resolve_path (path_segments, mode, tl::nullopt, ns_first,
			 ns_args...);
  }

  template <typename... Args>
  tl::optional<Rib::Definition> resolve_path (const AST::SimplePath &path,
					      Args &&...args)
  {
    return resolve_path (path.get_segments (),
			 path.has_opening_scope_resolution (),
			 std::forward<Args> (args)...);
  }

  template <typename... Args>
  tl::optional<Rib::Definition> resolve_path (const AST::PathInExpression &path,
					      Args &&...args)
  {
    return resolve_path (path.get_segments (), path.opening_scope_resolution (),
			 std::forward<Args> (args)...);
  }

  template <typename... Args>
  tl::optional<Rib::Definition> resolve_path (const AST::TypePath &path,
					      Args &&...args)
  {
    return resolve_path (path.get_segments (),
			 path.has_opening_scope_resolution_op (),
			 std::forward<Args> (args)...);
  }

private:
  /* Map of "usage" nodes which have been resolved to a "definition" node */
  std::map<Usage, Definition> resolved_nodes;
};

} // namespace Resolver2_0
} // namespace Rust

#endif // ! RUST_NAME_RESOLVER_2_0_H
