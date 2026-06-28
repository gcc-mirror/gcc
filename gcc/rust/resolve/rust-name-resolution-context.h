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

#ifndef RUST_NAME_RESOLVER_2_0_CTX_H
#define RUST_NAME_RESOLVER_2_0_CTX_H

#include "optional.h"
#include "rust-forever-stack.h"
#include "rust-hir-map.h"
#include "rust-rib.h"
#include "rust-stacked-contexts.h"
#include "rust-item.h"
#include "rust-name-resolution.h"

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

struct IdentifierMode
{
  bool is_ref;
  bool is_mut;

  IdentifierMode (bool is_ref, bool is_mut) : is_ref (is_ref), is_mut (is_mut)
  {}

  bool operator== (const IdentifierMode &other)
  {
    return other.is_ref == is_ref && other.is_mut == is_mut;
  }

  bool operator!= (const IdentifierMode &other) { return !(*this == other); }
};

struct Binding
{
  enum class Kind
  {
    Product,
    Or,
  } kind;

  // used to check the correctness of or-bindings
  bool has_expected_bindings;

  std::unordered_map<std::string, std::pair<location_t, IdentifierMode>> idents;

  Binding (Binding::Kind kind) : kind (kind), has_expected_bindings (false) {}
};

/**
 * Used to identify the source of a binding, and emit the correct error message.
 */
enum class BindingSource
{
  Match,
  Let,
  IfLet,
  WhileLet,
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

  void insert_ident (std::string ident, location_t locus, bool is_ref,
		     bool is_mut);

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
  virtual Resolver::CanonicalPath as_path (const NameResolutionContext &,
					   Namespace ns)
    = 0;

  virtual bool is_root () const = 0;

  virtual ~CanonicalPathRecord () = default;
};

class CanonicalPathRecordWithParent : public CanonicalPathRecord
{
public:
  CanonicalPathRecordWithParent (NodeId parent_node_id)
    : parent_node_id (parent_node_id)
  {}

  NodeId get_parent () { return parent_node_id; }

  bool is_root () const override final { return false; }

private:
  NodeId parent_node_id;
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

  Resolver::CanonicalPath as_path (const NameResolutionContext &,
				   Namespace ns) override;

  bool is_root () const override final { return true; }

private:
  NodeId node_id;
  CrateNum crate_num;
  std::string seg;
};

class CanonicalPathRecordNormal : public CanonicalPathRecordWithParent
{
public:
  CanonicalPathRecordNormal (NodeId parent_node_id, NodeId node_id,
			     std::string seg)
    : CanonicalPathRecordWithParent (parent_node_id), node_id (node_id),
      seg (std::move (seg))
  {
    rust_assert (!Analysis::Mappings::get ().node_is_crate (node_id));
  }

  Resolver::CanonicalPath as_path (const NameResolutionContext &,
				   Namespace ns) override;

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

  Resolver::CanonicalPath as_path (const NameResolutionContext &,
				   Namespace ns) override;

  bool is_root () const override final { return true; }

private:
  NodeId lookup_id;
  CanonicalPathRecord *cache;
};

class CanonicalPathRecordImpl : public CanonicalPathRecordWithParent
{
public:
  CanonicalPathRecordImpl (NodeId parent_node_id, NodeId impl_id,
			   NodeId type_id)
    : CanonicalPathRecordWithParent (parent_node_id), impl_id (impl_id),
      type_record (type_id)
  {}

  Resolver::CanonicalPath as_path (const NameResolutionContext &,
				   Namespace ns) override;

private:
  NodeId impl_id;
  CanonicalPathRecordLookup type_record;
};

class CanonicalPathRecordTraitImpl : public CanonicalPathRecordWithParent
{
public:
  CanonicalPathRecordTraitImpl (NodeId parent_node_id, NodeId impl_id,
				NodeId type_id, NodeId trait_path_id)
    : CanonicalPathRecordWithParent (parent_node_id), impl_id (impl_id),
      type_record (type_id), trait_path_record (trait_path_id)
  {}

  Resolver::CanonicalPath as_path (const NameResolutionContext &,
				   Namespace ns) override;

private:
  NodeId impl_id;
  CanonicalPathRecordLookup type_record;
  CanonicalPathRecordLookup trait_path_record;
};

class CanonicalPathCtx
{
public:
  CanonicalPathCtx (const NameResolutionContext &ctx)
    : current_record (UNKNOWN_NODEID), nr_ctx (&ctx)
  {}

  Resolver::CanonicalPath get_path (NodeId id, Namespace ns) const
  {
    return get_record (id).as_path (*nr_ctx, ns);
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
    rust_assert (current_record != UNKNOWN_NODEID);

    auto it = records.find (id);
    if (it == records.end ())
      {
	auto record
	  = new CanonicalPathRecordNormal (current_record, id, std::move (seg));
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
    rust_assert (current_record != UNKNOWN_NODEID);

    scope_inner (id, std::forward<F> (f), [this, id, &seg] () {
      return new CanonicalPathRecordNormal (current_record, id,
					    std::move (seg));
    });
  }

  template <typename F> void scope_impl (AST::InherentImpl &impl, F &&f)
  {
    rust_assert (current_record != UNKNOWN_NODEID);

    NodeId id = impl.get_node_id ();
    scope_inner (id, std::forward<F> (f), [this, id, &impl] () {
      return new CanonicalPathRecordImpl (current_record, id,
					  impl.get_type ().get_node_id ());
    });
  }

  template <typename F> void scope_impl (AST::TraitImpl &impl, F &&f)
  {
    rust_assert (current_record != UNKNOWN_NODEID);

    NodeId id = impl.get_node_id ();
    scope_inner (id, std::forward<F> (f), [this, id, &impl] () {
      return new CanonicalPathRecordTraitImpl (
	current_record, id, impl.get_type ().get_node_id (),
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

  /** Merge another CanonicalPathCtx within this one. Intended to be used when
   * merging crate name resolution context.
   */
  void merge (CanonicalPathCtx &&other)
  {
    records.insert (std::make_move_iterator (other.records.begin ()),
		    std::make_move_iterator (other.records.end ()));
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
		 || static_cast<CanonicalPathRecordWithParent &> (*it->second)
			.get_parent ()
		      == current_record);

    NodeId stash = it->first;
    std::swap (stash, current_record);

    std::forward<FCallback> (f_callback) ();

    std::swap (stash, current_record);
  }

  std::unordered_map<NodeId, std::unique_ptr<CanonicalPathRecord>> records;
  NodeId current_record;

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

  tl::expected<NodeId, DuplicateNameError>
  insert_variant (Identifier name, NodeId id, bool is_also_value);

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

  using Node = ForeverStackBase::Node;

  std::unique_ptr<Node> root;
  std::unique_ptr<Node> lang_prelude;
  std::unique_ptr<Node> extern_prelude;

  ForeverStack<Namespace::Values> values;
  ForeverStack<Namespace::Types> types;
  ForeverStack<Namespace::Macros> macros;
  ForeverStack<Namespace::Labels> labels;

  Analysis::Mappings &mappings;
  StackedContexts<BindingLayer> bindings;

  CanonicalPathCtx canonical_ctx;

  /**
   * The result type for a multi-namespace call to
   * NameResolutionContext::lookup()
   */
  struct NSLookup
  {
    NodeId id;
    Namespace ns;

    NSLookup (NodeId id, Namespace ns) : id (id), ns (ns) {}
  };

  /**
   * These functions are mostly useful for the FinalizedNameResolutionContext
   * and used in later passes of the pipeline. They don't need to know as much
   * about a definition, hence why they don't use the NamespacedDefinition which
   * returns a Rib::Definition.
   */
  void map_usage (Usage usage, Definition definition, Namespace ns);
  tl::optional<NodeId> lookup (NodeId usage, Namespace ns) const;

  /**
   * The order of namespaces is important - if the usage resolves in the first
   * namespace, then it will be returned. Collisions are not guarded against and
   * should NOT happen. This is for looking up usages once name resolution is
   * done and we are in later stages of the pipeline.
   */
  tl::optional<NSLookup> lookup (NodeId usage, Namespace ns1,
				 Namespace ns2) const;
  tl::optional<NSLookup> lookup (NodeId usage, Namespace ns1, Namespace ns2,
				 Namespace ns3) const;

  Resolver::CanonicalPath to_canonical_path (NodeId id, Namespace ns) const
  {
    return canonical_ctx.get_path (id, ns);
  }

  /**
   * The return value when the namespace in which a definition was resolved
   * matters
   */
  struct NamespacedDefinition
  {
    explicit NamespacedDefinition (Rib::Definition definition, Namespace ns)
      : definition (definition), ns (ns)
    {}

    static tl::optional<NamespacedDefinition>
    Maybe (tl::optional<Rib::Definition> definition, Namespace ns)
    {
      return definition.map ([ns] (Rib::Definition definition) {
	return NamespacedDefinition (definition, ns);
      });
    }

    Rib::Definition definition;
    Namespace ns;
  };

  tl::optional<NamespacedDefinition>
  resolve_path (const ResolutionPath &path, ResolutionMode mode,
		std::vector<Error> &collect_errors, Namespace ns)
  {
    std::function<void (Usage, Definition, Namespace)> insert_segment_resolution
      = [this] (Usage seg_id, Definition id, Namespace ns) {
	  map_usage (seg_id, id, ns);
	};

    tl::optional<NamespacedDefinition> resolved = tl::nullopt;

    switch (ns)
      {
      case Namespace::Values:
	resolved = NamespacedDefinition::Maybe (
	  resolve_path (values, path, mode, insert_segment_resolution,
			collect_errors),
	  ns);
	break;
      case Namespace::Types:
	resolved = NamespacedDefinition::Maybe (
	  resolve_path (types, path, mode, insert_segment_resolution,
			collect_errors),
	  ns);
	break;
      case Namespace::Macros:
	resolved = NamespacedDefinition::Maybe (
	  resolve_path (macros, path, mode, insert_segment_resolution,
			collect_errors),
	  ns);
	break;
      case Namespace::Labels:
	resolved = NamespacedDefinition::Maybe (
	  resolve_path (labels, path, mode, insert_segment_resolution,
			collect_errors),
	  ns);
	break;
      default:
	rust_unreachable ();
      }

    // If it fails, switch to std prelude resolution if it exists
    if (prelude && !resolved)
      {
	// TODO: Factor this with the above
	switch (ns)
	  {
	  case Namespace::Values:
	    return NamespacedDefinition::Maybe (
	      resolve_path (values, path, mode, insert_segment_resolution,
			    collect_errors, *prelude),
	      ns);
	  case Namespace::Types:
	    return NamespacedDefinition::Maybe (
	      resolve_path (types, path, mode, insert_segment_resolution,
			    collect_errors, *prelude),
	      ns);
	  case Namespace::Macros:
	    return NamespacedDefinition::Maybe (
	      resolve_path (macros, path, mode, insert_segment_resolution,
			    collect_errors, *prelude),
	      ns);
	  case Namespace::Labels:
	    return NamespacedDefinition::Maybe (
	      resolve_path (labels, path, mode, insert_segment_resolution,
			    collect_errors, *prelude),
	      ns);
	  default:
	    rust_unreachable ();
	  }
      }

    return resolved;
  }

  class ResolutionBuilder
  {
  public:
    ResolutionBuilder (NameResolutionContext &ctx) : ctx (&ctx) {}

    template <typename S>
    void set_path (const std::vector<S> &path_segments, NodeId node_id,
		   bool has_opening_scope)
    {
      path = ResolutionPath (path_segments, node_id);
      mode = ResolutionMode::Normal;
      if (has_opening_scope)
	{
	  if (get_rust_edition () == Edition::E2015)
	    mode = ResolutionMode::FromRoot;
	  else
	    mode = ResolutionMode::FromExtern;
	}
      has_path_set = true;
    }

    template <typename S>
    void set_path (const std::vector<S> &path_segments, NodeId node_id,
		   ResolutionMode mode)
    {
      path = ResolutionPath (path_segments, node_id);
      this->mode = mode;
      has_path_set = true;
    }

    void set_path (const AST::SimplePath &path)
    {
      set_path (path.get_segments (), path.get_node_id (),
		path.has_opening_scope_resolution ());
    }

    void set_path (const AST::PathInExpression &path)
    {
      set_path (path.get_segments (), path.get_node_id (),
		path.opening_scope_resolution ());
    }

    void set_path (const AST::TypePath &path)
    {
      set_path (path.get_segments (), path.get_node_id (),
		path.has_opening_scope_resolution_op ());
    }

    void set_mode (ResolutionMode mode) { this->mode = mode; }

    void add_namespaces (Namespace ns) { namespace_list.push_back (ns); }

    template <typename... Args> void add_namespaces (Namespace ns, Args... rest)
    {
      add_namespaces (ns);
      add_namespaces (rest...);
    }

    void set_collect_errors (tl::optional<std::vector<Error> &> collect_errors)
    {
      this->collect_errors = collect_errors;
    }

    tl::optional<NamespacedDefinition> resolve ()
    {
      rust_assert (has_path_set);

      for (auto ns : namespace_list)
	{
	  std::vector<Error> collect_errors_inner;
	  if (auto ret
	      = ctx->resolve_path (path, mode, collect_errors_inner, ns))
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
	    }
	}

      return tl::nullopt;
    }

  private:
    ResolutionPath path;
    ResolutionMode mode;
    bool has_path_set;

    std::vector<Namespace> namespace_list;

    tl::optional<std::vector<Error> &> collect_errors;

    NameResolutionContext *ctx;
  };

  template <typename S, typename... Args>
  tl::optional<NamespacedDefinition>
  resolve_path (const std::vector<S> &path_segments, ResolutionMode mode,
		tl::optional<std::vector<Error> &> collect_errors,
		Namespace ns_first, Args... ns_args)
  {
    ResolutionBuilder builder (*this);
    builder.set_path (path_segments, UNKNOWN_NODEID, mode);
    builder.add_namespaces (ns_first, ns_args...);
    builder.set_collect_errors (collect_errors);

    return builder.resolve ();
  }

  template <typename S, typename... Args>
  tl::optional<NamespacedDefinition>
  resolve_path (const std::vector<S> &path_segments,
		bool has_opening_scope_resolution,
		tl::optional<std::vector<Error> &> collect_errors,
		Namespace ns_first, Args... ns_args)
  {
    ResolutionBuilder builder (*this);
    builder.set_path (path_segments, UNKNOWN_NODEID,
		      has_opening_scope_resolution);
    builder.add_namespaces (ns_first, ns_args...);
    builder.set_collect_errors (collect_errors);

    return builder.resolve ();
  }

  template <typename S, typename... Args>
  tl::optional<NamespacedDefinition>
  resolve_path (const std::vector<S> &path_segments,
		bool has_opening_scope_resolution, Namespace ns_first,
		Args... ns_args)
  {
    ResolutionBuilder builder (*this);
    builder.set_path (path_segments, UNKNOWN_NODEID,
		      has_opening_scope_resolution);
    builder.add_namespaces (ns_first, ns_args...);

    return builder.resolve ();
  }

  template <typename S, typename... Args>
  tl::optional<NamespacedDefinition>
  resolve_path (const std::vector<S> &path_segments, ResolutionMode mode,
		Namespace ns_first, Args... ns_args)
  {
    ResolutionBuilder builder (*this);
    builder.set_path (path_segments, UNKNOWN_NODEID, mode);
    builder.add_namespaces (ns_first, ns_args...);

    return builder.resolve ();
  }

  template <typename... Args>
  tl::optional<NamespacedDefinition> resolve_path (const AST::SimplePath &path,
						   Args &&...args)
  {
    return resolve_path (path.get_segments (),
			 path.has_opening_scope_resolution (),
			 std::forward<Args> (args)...);
  }

  template <typename... Args>
  tl::optional<NamespacedDefinition>
  resolve_path (const AST::PathInExpression &path, Args &&...args)
  {
    return resolve_path (path.get_segments (), path.opening_scope_resolution (),
			 std::forward<Args> (args)...);
  }

  template <typename... Args>
  tl::optional<NamespacedDefinition> resolve_path (const AST::TypePath &path,
						   Args &&...args)
  {
    return resolve_path (path.get_segments (),
			 path.has_opening_scope_resolution_op (),
			 std::forward<Args> (args)...);
  }

  /*
   * Merge a name resolution context within another one at a given location.
   *
   * @param other The other name resolution context to merge within the current
   * one.
   * @param at The node id of the container were the nr context should be
   * merged. Usually an extern crate node.
   */
  void merge (NameResolutionContext &other, NodeId at);

// We disable this function for now as it causes regressions, but I think it
// is important for a more proper final nameres context - need to investigate
#if 0
  /**
   * We've now collected every definition and import, and errored out when
   * necessary if multiple definitions are colliding. Do a final flattening of
   * the name resolution context to make it easier to digest for the late name
   * resolution and type-checker. This basically turns the `resolved_nodes`
   * map from a linked-list-like map to a regular, flat hashmap.
   *
   * FIXME: The documentation is wrong, this needs to also run after all
   * usages have been *resolved* so after Late as well!!!
   *
   * TODO: Should this return something like the FinalizedNameResolutionCtx?
   * Or set it up at least? And instead of mutating the `resolved_nodes` map,
   * create a new one for the FinalizedNameResolutionCtx?
   * Actually, since Late uses the NRCtx directly we should mutate this. Most
   * later passes don't look at this map. So let's go for side-effects in a
   * void function, yipee.
   */
  void flatten ();
#endif

  /* If declared with #[prelude_import], the current standard library module
   */
  tl::optional<NodeId> prelude;

private:
  template <Namespace N>
  bool
  should_search_prelude (const typename ForeverStack<N>::Node *current_node,
			 const typename ForeverStack<N>::SegIterator &iterator,
			 const std::vector<ResolutionPath::Segment> &segments);

  /**
   * Resolve a path to its definition
   *
   * // TODO: Add documentation for `segments`
   *
   * @return a valid option with the Definition if the path is present in the
   *         current map, an empty one otherwise.
   */
  template <Namespace N>
  tl::optional<Rib::Definition>
  resolve_path (ForeverStack<N> &stack, const ResolutionPath &path,
		ResolutionMode mode,
		std::function<void (Usage, Definition, Namespace)>
		  insert_segment_resolution,
		std::vector<Error> &collect_errors);

  template <Namespace N>
  tl::optional<Rib::Definition>
  resolve_path (ForeverStack<N> &stack, const ResolutionPath &path,
		ResolutionMode mode,
		std::function<void (Usage, Definition, Namespace)>
		  insert_segment_resolution,
		std::vector<Error> &collect_errors, NodeId starting_point_id);

  template <Namespace N>
  tl::optional<Rib::Definition> resolve_path (
    ForeverStack<N> &stack, const ResolutionPath &path, ResolutionMode mode,
    std::function<void (Usage, Definition, Namespace)>
      insert_segment_resolution,
    std::vector<Error> &collect_errors,
    std::reference_wrapper<typename ForeverStack<N>::Node> starting_point);

  template <Namespace N>
  tl::optional<typename ForeverStack<N>::Node &>
  resolve_segments (ForeverStack<N> &stack,
		    typename ForeverStack<N>::Node &starting_point,
		    const std::vector<ResolutionPath::Segment> &segments,
		    typename ForeverStack<N>::SegIterator iterator,
		    std::function<void (Usage, Definition, Namespace)>
		      insert_segment_resolution,
		    std::vector<Error> &collect_errors);

  template <Namespace N>
  tl::optional<Rib::Definition>
  resolve_final_segment (ForeverStack<N> &stack,
			 typename ForeverStack<N>::Node &final_node,
			 std::string &seg_name, bool is_lower_self);
};

} // namespace Resolver2_0
} // namespace Rust

#include "rust-name-resolution-context.hxx"

#endif // ! RUST_NAME_RESOLVER_2_0_CTX_H
