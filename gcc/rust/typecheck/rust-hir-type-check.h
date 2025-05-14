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

#ifndef RUST_HIR_TYPE_CHECK
#define RUST_HIR_TYPE_CHECK

#include "rust-hir-map.h"
#include "rust-tyty.h"
#include "rust-hir-trait-reference.h"
#include "rust-stacked-contexts.h"
#include "rust-autoderef.h"
#include "rust-tyty-region.h"
#include "rust-tyty-variance-analysis.h"
#include "rust-system.h"

namespace Rust {
namespace Resolver {

class TypeCheckContextItem
{
public:
  enum ItemType
  {
    ITEM,
    IMPL_ITEM,
    TRAIT_ITEM,
    ERROR
  };

  TypeCheckContextItem (HIR::Function *item);
  TypeCheckContextItem (HIR::ImplBlock &impl_block, HIR::Function *item);
  TypeCheckContextItem (HIR::TraitItemFunc *trait_item);
  TypeCheckContextItem (const TypeCheckContextItem &other);

  TypeCheckContextItem &operator= (const TypeCheckContextItem &other);

  static TypeCheckContextItem get_error ();

  bool is_error () const;

  ItemType get_type () const;

  HIR::Function *get_item ();

  std::pair<HIR::ImplBlock *, HIR::Function *> &get_impl_item ();

  HIR::TraitItemFunc *get_trait_item ();

  TyTy::FnType *get_context_type ();

  DefId get_defid () const;

private:
  TypeCheckContextItem ();

  union Item
  {
    HIR::Function *item;
    std::pair<HIR::ImplBlock *, HIR::Function *> impl_item;
    HIR::TraitItemFunc *trait_item;

    Item (HIR::Function *item);
    Item (HIR::ImplBlock *impl_block, HIR::Function *item);
    Item (HIR::TraitItemFunc *trait_item);
  };

  ItemType type;
  Item item;
};

class TypeCheckBlockContextItem
{
public:
  enum ItemType
  {
    IMPL_BLOCK,
    TRAIT
  };

  TypeCheckBlockContextItem (HIR::ImplBlock *block);
  TypeCheckBlockContextItem (HIR::Trait *trait);

  bool is_impl_block () const;
  bool is_trait_block () const;

  HIR::ImplBlock &get_impl_block ();
  HIR::Trait &get_trait ();

private:
  union Item
  {
    HIR::ImplBlock *block;
    HIR::Trait *trait;

    Item (HIR::ImplBlock *block);
    Item (HIR::Trait *trait);
  };
  ItemType type;
  Item item;
};

/**
 * Interned lifetime representation in TyTy
 *
 * On the HIR->TyTy boundary HIR::Lifetime is interned into this struct.
 */
class Lifetime
{
  uint32_t interner_index;

public:
  explicit constexpr Lifetime (uint32_t interner_index)
    : interner_index (interner_index)
  {}

  Lifetime () = default;

  WARN_UNUSED_RESULT bool is_static () const { return interner_index == 0; }

  WARN_UNUSED_RESULT static constexpr Lifetime static_lifetime ()
  {
    return Lifetime (0);
  }

  WARN_UNUSED_RESULT static constexpr Lifetime anonymous_lifetime ()
  {
    return Lifetime (1);
  }

  static constexpr uint32_t FIRST_NAMED_LIFETIME = 2;

  friend bool operator== (const Lifetime &lhs, const Lifetime &rhs)
  {
    return lhs.interner_index == rhs.interner_index;
  }

  friend bool operator!= (const Lifetime &lhs, const Lifetime &rhs)
  {
    return !(lhs == rhs);
  }

  WARN_UNUSED_RESULT Lifetime next () { return Lifetime (interner_index++); }
};

class TypeCheckContext
{
public:
  static TypeCheckContext *get ();

  ~TypeCheckContext ();

  bool lookup_builtin (NodeId id, TyTy::BaseType **type);
  bool lookup_builtin (std::string name, TyTy::BaseType **type);
  void insert_builtin (HirId id, NodeId ref, TyTy::BaseType *type);
  const std::vector<std::unique_ptr<TyTy::BaseType>> &get_builtins () const;

  void insert_type (const Analysis::NodeMapping &mappings,
		    TyTy::BaseType *type);
  bool lookup_type (HirId id, TyTy::BaseType **type) const;
  void clear_type (TyTy::BaseType *ty);

  void insert_implicit_type (HirId id, TyTy::BaseType *type);

  void insert_type_by_node_id (NodeId ref, HirId id);
  bool lookup_type_by_node_id (NodeId ref, HirId *id);

  bool have_function_context () const;
  TyTy::BaseType *peek_return_type ();
  TypeCheckContextItem peek_context ();
  void push_return_type (TypeCheckContextItem item,
			 TyTy::BaseType *return_type);
  void pop_return_type ();

  StackedContexts<TypeCheckBlockContextItem> &block_context ();

  void iterate (std::function<bool (HirId, TyTy::BaseType *)> cb);

  bool have_loop_context () const;
  void push_new_loop_context (HirId id, location_t locus);
  void push_new_while_loop_context (HirId id);
  TyTy::BaseType *peek_loop_context ();
  TyTy::BaseType *pop_loop_context ();

  void swap_head_loop_context (TyTy::BaseType *val);

  void insert_trait_reference (DefId id, TraitReference &&ref);
  bool lookup_trait_reference (DefId id, TraitReference **ref);

  void insert_associated_trait_impl (HirId id,
				     AssociatedImplTrait &&associated);
  bool lookup_associated_trait_impl (HirId id,
				     AssociatedImplTrait **associated);

  void insert_associated_type_mapping (HirId id, HirId mapping);
  void clear_associated_type_mapping (HirId id);

  // lookup any associated type mappings, the out parameter of mapping is
  // allowed to be nullptr which allows this interface to do a simple does exist
  // check
  bool lookup_associated_type_mapping (HirId id, HirId *mapping);

  void insert_associated_impl_mapping (HirId trait_id,
				       const TyTy::BaseType *impl_type,
				       HirId impl_id);
  bool lookup_associated_impl_mapping_for_self (HirId trait_id,
						const TyTy::BaseType *self,
						HirId *mapping);

  void insert_autoderef_mappings (HirId id,
				  std::vector<Adjustment> &&adjustments);
  bool lookup_autoderef_mappings (HirId id,
				  std::vector<Adjustment> **adjustments);

  void insert_cast_autoderef_mappings (HirId id,
				       std::vector<Adjustment> &&adjustments);
  bool lookup_cast_autoderef_mappings (HirId id,
				       std::vector<Adjustment> **adjustments);

  void insert_variant_definition (HirId id, HirId variant);
  bool lookup_variant_definition (HirId id, HirId *variant);

  void insert_operator_overload (HirId id, TyTy::FnType *call_site);
  bool lookup_operator_overload (HirId id, TyTy::FnType **call);

  void insert_unconstrained_check_marker (HirId id, bool status);
  bool have_checked_for_unconstrained (HirId id, bool *result);

  void insert_resolved_predicate (HirId id, TyTy::TypeBoundPredicate predicate);
  bool lookup_predicate (HirId id, TyTy::TypeBoundPredicate *result);

  void insert_query (HirId id);
  void query_completed (HirId id);
  bool query_in_progress (HirId id) const;

  void insert_trait_query (DefId id);
  void trait_query_completed (DefId id);
  bool trait_query_in_progress (DefId id) const;

  Lifetime intern_lifetime (const HIR::Lifetime &name);
  WARN_UNUSED_RESULT tl::optional<Lifetime>
  lookup_lifetime (const HIR::Lifetime &lifetime) const;

  WARN_UNUSED_RESULT tl::optional<TyTy::Region>
  lookup_and_resolve_lifetime (const HIR::Lifetime &lifetime) const;

  void intern_and_insert_lifetime (const HIR::Lifetime &lifetime);

  WARN_UNUSED_RESULT std::vector<TyTy::Region>
  regions_from_generic_args (const HIR::GenericArgs &args) const;

  void compute_inference_variables (bool error);

  TyTy::VarianceAnalysis::CrateCtx &get_variance_analysis_ctx ();

private:
  TypeCheckContext ();

  std::map<NodeId, HirId> node_id_refs;
  std::map<HirId, TyTy::BaseType *> resolved;
  std::vector<std::unique_ptr<TyTy::BaseType>> builtins;
  std::vector<std::pair<TypeCheckContextItem, TyTy::BaseType *>>
    return_type_stack;
  std::vector<TyTy::BaseType *> loop_type_stack;
  StackedContexts<TypeCheckBlockContextItem> block_stack;
  std::map<DefId, TraitReference> trait_context;
  std::map<HirId, AssociatedImplTrait> associated_impl_traits;

  // trait-id -> list of < self-tyty:impl-id>
  std::map<HirId, std::vector<std::pair<const TyTy::BaseType *, HirId>>>
    associated_traits_to_impls;

  std::map<HirId, HirId> associated_type_mappings;

  // adjustment mappings
  std::map<HirId, std::vector<Adjustment>> autoderef_mappings;
  std::map<HirId, std::vector<Adjustment>> cast_autoderef_mappings;

  // operator overloads
  std::map<HirId, TyTy::FnType *> operator_overloads;

  // variants
  std::map<HirId, HirId> variants;

  // unconstrained type-params check
  std::map<HirId, bool> unconstrained;

  // predicates
  std::map<HirId, TyTy::TypeBoundPredicate> predicates;

  // query context lookups
  std::set<HirId> querys_in_progress;
  std::set<DefId> trait_queries_in_progress;

  // variance analysis
  TyTy::VarianceAnalysis::CrateCtx variance_analysis_ctx;

  /** Used to resolve (interned) lifetime names to their bounding scope. */
  class LifetimeResolver
  {
    /**
     * The level of nested scopes, where the lifetime was declared.
     *
     * Index 0 is used for `impl` blocks and is skipped if not explicitly
     * requested.
     * Index 1 for the top-level of declarations of items.
     * Index >1 is used for late-bound lifetimes.
     */
    using ScopeIndex = size_t;

    static constexpr ScopeIndex IMPL_SCOPE = 0;
    static constexpr ScopeIndex ITEM_SCOPE = 1;

    /**
     * A reference to a lifetime binder.
     *
     * This is used to resolve lifetimes to their scope.
     */
    struct LifetimeBinderRef
    {
      uint32_t scope; //> Depth of the scope where the lifetime was declared.
      uint32_t index; //> Index of the lifetime in the scope.
    };

    /**
     * A stack of the number of lifetimes declared in each scope.
     *
     * Used to pop the correct number of lifetimes when leaving a scope.
     */
    std::stack<uint32_t> binder_size_stack;

    /**
     * Merged stack of all lifetimes declared in all scopes.
     *
     * Use `binder_size_stack` to determine the number of lifetimes in each
     * scope.
     */
    std::vector<std::pair<Lifetime, LifetimeBinderRef>> lifetime_lookup;

    /**
     * Whether the current scope is a function body.
     *
     * In function header, lifetimes are resolved as early-bound, in the body as
     * named. This is because the header can be also used in call position.
     */
    bool is_body = false;

    /** Return the number of the current scope. */
    WARN_UNUSED_RESULT uint32_t get_current_scope () const
    {
      return binder_size_stack.size () - 1;
    }

  public:
    /** Add new declaration of a lifetime. */
    void insert_mapping (Lifetime placeholder)
    {
      lifetime_lookup.push_back (
	{placeholder, {get_current_scope (), binder_size_stack.top ()++}});
    }

    WARN_UNUSED_RESULT tl::optional<TyTy::Region>
    resolve (const Lifetime &placeholder) const;

    /** Only to be used by the guard. */
    void push_binder () { binder_size_stack.push (0); }
    /** Only to be used by the guard. */
    void pop_binder () { binder_size_stack.pop (); }

    bool binder_empty () { return binder_size_stack.empty (); }

    /**
     * Switch from resolving a function header to a function body.
     */
    void switch_to_fn_body () { this->is_body = true; }

    size_t get_num_bound_regions () const { return binder_size_stack.top (); }
  };

  // lifetime resolving
  std::unordered_map<std::string, Lifetime> lifetime_name_interner;
  Lifetime next_lifetime_index = Lifetime (Lifetime::FIRST_NAMED_LIFETIME);

  /**
   * Stack of lifetime resolvers.
   *
   * Due to the contruction of the type checker, it is possible to start
   * resolution of a new type in the middle of resolving another type. This
   * stack isolates the conexts in such cases.
   */
  std::stack<LifetimeResolver> lifetime_resolver_stack;

public:
  WARN_UNUSED_RESULT LifetimeResolver &get_lifetime_resolver ()
  {
    rust_assert (!lifetime_resolver_stack.empty ());
    return lifetime_resolver_stack.top ();
  }

  WARN_UNUSED_RESULT const LifetimeResolver &get_lifetime_resolver () const
  {
    rust_assert (!lifetime_resolver_stack.empty ());
    return lifetime_resolver_stack.top ();
  }

  /**
   * A guard that pushes a new lifetime resolver on the stack and pops it
   * when it goes out of scope.
   */
  class LifetimeResolverGuard
  {
  public:
    /** The kind of scope that is being pushed. */
    enum ScopeKind
    {
      IMPL_BLOCK_RESOLVER, //> A new `impl` block scope.
      RESOLVER,		   //> A new scope for a function body.
      BINDER,		   //> A new scope for late-bound lifetimes.
    };

  private:
    TypeCheckContext &ctx;
    ScopeKind kind;

  public:
    LifetimeResolverGuard (TypeCheckContext &ctx, ScopeKind kind)
      : ctx (ctx), kind (kind)
    {
      if (kind == IMPL_BLOCK_RESOLVER)
	{
	  ctx.lifetime_resolver_stack.push (LifetimeResolver ());
	}

      if (kind == RESOLVER)
	{
	  ctx.lifetime_resolver_stack.push (LifetimeResolver ());
	  // Skip the `impl` block scope.
	  ctx.lifetime_resolver_stack.top ().push_binder ();
	}
      rust_assert (!ctx.lifetime_resolver_stack.empty ());
      ctx.lifetime_resolver_stack.top ().push_binder ();
    }

    ~LifetimeResolverGuard ()
    {
      rust_assert (!ctx.lifetime_resolver_stack.empty ());
      if (!ctx.lifetime_resolver_stack.top ().binder_empty ())
	ctx.lifetime_resolver_stack.top ().pop_binder ();
      if (kind == RESOLVER)
	{
	  ctx.lifetime_resolver_stack.pop ();
	}
    }
  };

  /** Start new late bound lifetime scope. */
  WARN_UNUSED_RESULT LifetimeResolverGuard push_lifetime_binder ()
  {
    return LifetimeResolverGuard (*this, LifetimeResolverGuard::BINDER);
  }

  /** Start new function body scope. */
  WARN_UNUSED_RESULT LifetimeResolverGuard
  push_clean_lifetime_resolver (bool is_impl_block = false)
  {
    return LifetimeResolverGuard (*this,
				  is_impl_block
				    ? LifetimeResolverGuard::IMPL_BLOCK_RESOLVER
				    : LifetimeResolverGuard::RESOLVER);
  }

  /** Switch from resolving a function header to a function body. */
  void switch_to_fn_body ()
  {
    this->lifetime_resolver_stack.top ().switch_to_fn_body ();
  }
};

class TypeResolution
{
public:
  static void Resolve (HIR::Crate &crate);
};

class TraitQueryGuard
{
public:
  TraitQueryGuard (DefId id) : id (id), ctx (*TypeCheckContext::get ())
  {
    ctx.insert_trait_query (id);
  }

  ~TraitQueryGuard () { ctx.trait_query_completed (id); }

private:
  DefId id;
  TypeCheckContext &ctx;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK
