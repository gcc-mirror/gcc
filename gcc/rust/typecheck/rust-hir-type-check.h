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

#ifndef RUST_HIR_TYPE_CHECK
#define RUST_HIR_TYPE_CHECK

#include "rust-type-util.h"
#include "rust-hir-full-decls.h"
#include "rust-hir-map.h"
#include "rust-tyty.h"
#include "rust-hir-trait-reference.h"
#include "rust-autoderef.h"

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
  };

  TypeCheckContextItem (HIR::Function *item);
  TypeCheckContextItem (HIR::ImplBlock *impl_block, HIR::Function *item);
  TypeCheckContextItem (HIR::TraitItemFunc *trait_item);

  ItemType get_type () const;

  HIR::Function *get_item ();

  std::pair<HIR::ImplBlock *, HIR::Function *> &get_impl_item ();

  HIR::TraitItemFunc *get_trait_item ();

  TyTy::FnType *get_context_type ();

private:
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

class TypeCheckContext
{
public:
  static TypeCheckContext *get ();

  ~TypeCheckContext ();

  bool lookup_builtin (NodeId id, TyTy::BaseType **type);
  bool lookup_builtin (std::string name, TyTy::BaseType **type);
  void insert_builtin (HirId id, NodeId ref, TyTy::BaseType *type);

  void insert_type (const Analysis::NodeMapping &mappings,
		    TyTy::BaseType *type);
  void insert_implicit_type (TyTy::BaseType *type);
  bool lookup_type (HirId id, TyTy::BaseType **type) const;

  void insert_implicit_type (HirId id, TyTy::BaseType *type);

  void insert_type_by_node_id (NodeId ref, HirId id);
  bool lookup_type_by_node_id (NodeId ref, HirId *id);

  TyTy::BaseType *peek_return_type ();
  TypeCheckContextItem &peek_context ();
  void push_return_type (TypeCheckContextItem item,
			 TyTy::BaseType *return_type);
  void pop_return_type ();
  void iterate (std::function<bool (HirId, TyTy::BaseType *)> cb);

  bool have_loop_context () const;
  void push_new_loop_context (HirId id, Location locus);
  void push_new_while_loop_context (HirId id);
  TyTy::BaseType *peek_loop_context ();
  TyTy::BaseType *pop_loop_context ();

  void swap_head_loop_context (TyTy::BaseType *val);

  void insert_trait_reference (DefId id, TraitReference &&ref);
  bool lookup_trait_reference (DefId id, TraitReference **ref);

  void insert_receiver (HirId id, TyTy::BaseType *t);
  bool lookup_receiver (HirId id, TyTy::BaseType **ref);

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

private:
  TypeCheckContext ();

  std::map<NodeId, HirId> node_id_refs;
  std::map<HirId, TyTy::BaseType *> resolved;
  std::vector<std::unique_ptr<TyTy::BaseType>> builtins;
  std::vector<std::pair<TypeCheckContextItem, TyTy::BaseType *>>
    return_type_stack;
  std::vector<TyTy::BaseType *> loop_type_stack;
  std::map<DefId, TraitReference> trait_context;
  std::map<HirId, TyTy::BaseType *> receiver_context;
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
