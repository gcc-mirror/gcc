// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#include "rust-hir-full-decls.h"
#include "rust-hir-map.h"
#include "rust-tyty.h"
#include "rust-hir-trait-ref.h"
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

  TypeCheckContextItem (HIR::Function *item)
    : type (ItemType::ITEM), item (item)
  {}

  TypeCheckContextItem (HIR::ImplBlock *impl_block, HIR::Function *item)
    : type (ItemType::IMPL_ITEM), item (impl_block, item)
  {}

  TypeCheckContextItem (HIR::TraitItemFunc *trait_item)
    : type (ItemType::TRAIT_ITEM), item (trait_item)
  {}

  ItemType get_type () const { return type; }

  HIR::Function *get_item ()
  {
    rust_assert (get_type () == ItemType::ITEM);
    return item.item;
  }

  std::pair<HIR::ImplBlock *, HIR::Function *> &get_impl_item ()
  {
    rust_assert (get_type () == ItemType::IMPL_ITEM);
    return item.impl_item;
  };

  HIR::TraitItemFunc *get_trait_item ()
  {
    rust_assert (get_type () == ItemType::TRAIT_ITEM);
    return item.trait_item;
  }

private:
  union Item
  {
    HIR::Function *item;
    std::pair<HIR::ImplBlock *, HIR::Function *> impl_item;
    HIR::TraitItemFunc *trait_item;

    Item (HIR::Function *item) : item (item) {}

    Item (HIR::ImplBlock *impl_block, HIR::Function *item)
      : impl_item ({impl_block, item})
    {}

    Item (HIR::TraitItemFunc *trait_item) : trait_item (trait_item) {}
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
  bool lookup_type (HirId id, TyTy::BaseType **type);

  void insert_implicit_type (HirId id, TyTy::BaseType *type);

  void insert_type_by_node_id (NodeId ref, HirId id);
  bool lookup_type_by_node_id (NodeId ref, HirId *id);

  TyTy::BaseType *peek_return_type ();
  TypeCheckContextItem &peek_context ();
  void push_return_type (TypeCheckContextItem item,
			 TyTy::BaseType *return_type);
  void pop_return_type ();

  void iterate (std::function<bool (HirId, TyTy::BaseType *)> cb)
  {
    for (auto it = resolved.begin (); it != resolved.end (); it++)
      {
	if (!cb (it->first, it->second))
	  return;
      }
  }

  void push_new_loop_context (HirId id, Location locus)
  {
    TyTy::BaseType *infer_var
      = new TyTy::InferType (id, TyTy::InferType::InferTypeKind::GENERAL,
			     locus);
    loop_type_stack.push_back (infer_var);
  }

  void push_new_while_loop_context (HirId id)
  {
    TyTy::BaseType *infer_var = new TyTy::ErrorType (id);
    loop_type_stack.push_back (infer_var);
  }

  TyTy::BaseType *peek_loop_context () { return loop_type_stack.back (); }

  TyTy::BaseType *pop_loop_context ()
  {
    auto back = peek_loop_context ();
    loop_type_stack.pop_back ();
    return back;
  }

  void swap_head_loop_context (TyTy::BaseType *val)
  {
    loop_type_stack.pop_back ();
    loop_type_stack.push_back (val);
  }

  void insert_trait_reference (DefId id, TraitReference &&ref)
  {
    rust_assert (trait_context.find (id) == trait_context.end ());
    trait_context.emplace (id, std::move (ref));
  }

  bool lookup_trait_reference (DefId id, TraitReference **ref)
  {
    auto it = trait_context.find (id);
    if (it == trait_context.end ())
      return false;

    *ref = &it->second;
    return true;
  }

  void insert_receiver (HirId id, TyTy::BaseType *t)
  {
    receiver_context[id] = t;
  }

  bool lookup_receiver (HirId id, TyTy::BaseType **ref)
  {
    auto it = receiver_context.find (id);
    if (it == receiver_context.end ())
      return false;

    *ref = it->second;
    return true;
  }

  void insert_associated_trait_impl (HirId id, AssociatedImplTrait &&associated)
  {
    rust_assert (associated_impl_traits.find (id)
		 == associated_impl_traits.end ());
    associated_impl_traits.emplace (id, std::move (associated));
  }

  bool lookup_associated_trait_impl (HirId id, AssociatedImplTrait **associated)
  {
    auto it = associated_impl_traits.find (id);
    if (it == associated_impl_traits.end ())
      return false;

    *associated = &it->second;
    return true;
  }

  void insert_associated_type_mapping (HirId id, HirId mapping)
  {
    associated_type_mappings[id] = mapping;
  }

  void clear_associated_type_mapping (HirId id)
  {
    auto it = associated_type_mappings.find (id);
    rust_assert (it != associated_type_mappings.end ());
    associated_type_mappings.erase (it);
  }

  // lookup any associated type mappings, the out parameter of mapping is
  // allowed to be nullptr which allows this interface to do a simple does exist
  // check
  bool lookup_associated_type_mapping (HirId id, HirId *mapping)
  {
    auto it = associated_type_mappings.find (id);
    if (it == associated_type_mappings.end ())
      return false;

    if (mapping != nullptr)
      *mapping = it->second;

    return true;
  }

  void insert_associated_impl_mapping (HirId trait_id,
				       const TyTy::BaseType *impl_type,
				       HirId impl_id)
  {
    auto it = associated_traits_to_impls.find (trait_id);
    if (it == associated_traits_to_impls.end ())
      {
	associated_traits_to_impls[trait_id] = {};
      }

    associated_traits_to_impls[trait_id].push_back ({impl_type, impl_id});
  }

  bool lookup_associated_impl_mapping_for_self (HirId trait_id,
						const TyTy::BaseType *self,
						HirId *mapping)
  {
    auto it = associated_traits_to_impls.find (trait_id);
    if (it == associated_traits_to_impls.end ())
      return false;

    for (auto &item : it->second)
      {
	if (item.first->can_eq (self, false))
	  {
	    *mapping = item.second;
	    return true;
	  }
      }
    return false;
  }

  void insert_autoderef_mappings (HirId id,
				  std::vector<Adjustment> &&adjustments)
  {
    rust_assert (autoderef_mappings.find (id) == autoderef_mappings.end ());
    autoderef_mappings.emplace (id, std::move (adjustments));
  }

  bool lookup_autoderef_mappings (HirId id,
				  std::vector<Adjustment> **adjustments)
  {
    auto it = autoderef_mappings.find (id);
    if (it == autoderef_mappings.end ())
      return false;

    *adjustments = &it->second;
    return true;
  }

  void insert_variant_definition (HirId id, HirId variant)
  {
    auto it = variants.find (id);
    rust_assert (it == variants.end ());

    variants[id] = variant;
  }

  bool lookup_variant_definition (HirId id, HirId *variant)
  {
    auto it = variants.find (id);
    if (it == variants.end ())
      return false;

    *variant = it->second;
    return true;
  }

  void insert_operator_overload (HirId id, TyTy::FnType *call_site)
  {
    auto it = operator_overloads.find (id);
    rust_assert (it == operator_overloads.end ());

    operator_overloads[id] = call_site;
  }

  bool lookup_operator_overload (HirId id, TyTy::FnType **call)
  {
    auto it = operator_overloads.find (id);
    if (it == operator_overloads.end ())
      return false;

    *call = it->second;
    return true;
  }

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

  // operator overloads
  std::map<HirId, TyTy::FnType *> operator_overloads;

  // variants
  std::map<HirId, HirId> variants;
};

class TypeResolution
{
public:
  static void Resolve (HIR::Crate &crate);
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK
