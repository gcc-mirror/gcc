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

#include "rust-hir-type-check.h"
#include "rust-type-util.h"

namespace Rust {
namespace Resolver {

TypeCheckContext *
TypeCheckContext::get ()
{
  static TypeCheckContext *instance;
  if (instance == nullptr)
    instance = new TypeCheckContext ();

  return instance;
}

TypeCheckContext::TypeCheckContext () { lifetime_resolver_stack.emplace (); }

TypeCheckContext::~TypeCheckContext () {}

bool
TypeCheckContext::lookup_builtin (NodeId id, TyTy::BaseType **type)
{
  auto ref_it = node_id_refs.find (id);
  if (ref_it == node_id_refs.end ())
    return false;

  auto it = resolved.find (ref_it->second);
  if (it == resolved.end ())
    return false;

  *type = it->second;
  return true;
}

bool
TypeCheckContext::lookup_builtin (std::string name, TyTy::BaseType **type)
{
  for (auto &builtin : builtins)
    {
      if (name.compare (builtin->as_string ()) == 0)
	{
	  *type = builtin.get ();
	  return true;
	}
    }
  return false;
}

void
TypeCheckContext::insert_builtin (HirId id, NodeId ref, TyTy::BaseType *type)
{
  node_id_refs[ref] = id;
  resolved[id] = type;
  builtins.push_back (std::unique_ptr<TyTy::BaseType> (type));
}

void
TypeCheckContext::insert_type (const Analysis::NodeMapping &mappings,
			       TyTy::BaseType *type)
{
  rust_assert (type != nullptr);
  NodeId ref = mappings.get_nodeid ();
  HirId id = mappings.get_hirid ();
  node_id_refs[ref] = id;
  resolved[id] = type;
}

void
TypeCheckContext::insert_implicit_type (TyTy::BaseType *type)
{
  rust_assert (type != nullptr);
  resolved[type->get_ref ()] = type;
}

void
TypeCheckContext::insert_implicit_type (HirId id, TyTy::BaseType *type)
{
  rust_assert (type != nullptr);
  resolved[id] = type;
}

bool
TypeCheckContext::lookup_type (HirId id, TyTy::BaseType **type) const
{
  auto it = resolved.find (id);
  if (it == resolved.end ())
    return false;

  *type = it->second;
  return true;
}

void
TypeCheckContext::clear_type (TyTy::BaseType *ty)
{
  auto it = resolved.find (ty->get_ref ());
  if (it == resolved.end ())
    return;

  resolved.erase (it);
}

void
TypeCheckContext::insert_type_by_node_id (NodeId ref, HirId id)
{
  rust_assert (node_id_refs.find (ref) == node_id_refs.end ());
  node_id_refs[ref] = id;
}

bool
TypeCheckContext::lookup_type_by_node_id (NodeId ref, HirId *id)
{
  auto it = node_id_refs.find (ref);
  if (it == node_id_refs.end ())
    return false;

  *id = it->second;
  return true;
}

bool
TypeCheckContext::have_function_context () const
{
  return !return_type_stack.empty ();
}

TyTy::BaseType *
TypeCheckContext::peek_return_type ()
{
  rust_assert (!return_type_stack.empty ());
  return return_type_stack.back ().second;
}

void
TypeCheckContext::push_return_type (TypeCheckContextItem item,
				    TyTy::BaseType *return_type)
{
  return_type_stack.push_back ({std::move (item), return_type});
}

void
TypeCheckContext::pop_return_type ()
{
  rust_assert (!return_type_stack.empty ());
  return_type_stack.pop_back ();
}

TypeCheckContextItem
TypeCheckContext::peek_context ()
{
  rust_assert (!return_type_stack.empty ());
  return return_type_stack.back ().first;
}

void
TypeCheckContext::iterate (std::function<bool (HirId, TyTy::BaseType *)> cb)
{
  for (auto it = resolved.begin (); it != resolved.end (); it++)
    {
      if (!cb (it->first, it->second))
	return;
    }
}

bool
TypeCheckContext::have_loop_context () const
{
  return !loop_type_stack.empty ();
}

void
TypeCheckContext::push_new_loop_context (HirId id, location_t locus)
{
  TyTy::BaseType *infer_var
    = new TyTy::InferType (id, TyTy::InferType::InferTypeKind::GENERAL,
			   TyTy::InferType::TypeHint::Default (), locus);
  loop_type_stack.push_back (infer_var);
}

void
TypeCheckContext::push_new_while_loop_context (HirId id)
{
  TyTy::BaseType *infer_var = new TyTy::ErrorType (id);
  loop_type_stack.push_back (infer_var);
}

TyTy::BaseType *
TypeCheckContext::peek_loop_context ()
{
  return loop_type_stack.back ();
}

TyTy::BaseType *
TypeCheckContext::pop_loop_context ()
{
  auto back = peek_loop_context ();
  loop_type_stack.pop_back ();
  return back;
}

void
TypeCheckContext::swap_head_loop_context (TyTy::BaseType *val)
{
  loop_type_stack.pop_back ();
  loop_type_stack.push_back (val);
}

void
TypeCheckContext::insert_trait_reference (DefId id, TraitReference &&ref)
{
  rust_assert (trait_context.find (id) == trait_context.end ());
  trait_context.emplace (id, std::move (ref));
}

bool
TypeCheckContext::lookup_trait_reference (DefId id, TraitReference **ref)
{
  auto it = trait_context.find (id);
  if (it == trait_context.end ())
    return false;

  *ref = &it->second;
  return true;
}

void
TypeCheckContext::insert_receiver (HirId id, TyTy::BaseType *t)
{
  receiver_context[id] = t;
}

bool
TypeCheckContext::lookup_receiver (HirId id, TyTy::BaseType **ref)
{
  auto it = receiver_context.find (id);
  if (it == receiver_context.end ())
    return false;

  *ref = it->second;
  return true;
}

void
TypeCheckContext::insert_associated_trait_impl (
  HirId id, AssociatedImplTrait &&associated)
{
  rust_assert (associated_impl_traits.find (id)
	       == associated_impl_traits.end ());
  associated_impl_traits.emplace (id, std::move (associated));
}

bool
TypeCheckContext::lookup_associated_trait_impl (
  HirId id, AssociatedImplTrait **associated)
{
  auto it = associated_impl_traits.find (id);
  if (it == associated_impl_traits.end ())
    return false;

  *associated = &it->second;
  return true;
}

void
TypeCheckContext::insert_associated_type_mapping (HirId id, HirId mapping)
{
  associated_type_mappings[id] = mapping;
}

void
TypeCheckContext::clear_associated_type_mapping (HirId id)
{
  auto it = associated_type_mappings.find (id);
  if (it != associated_type_mappings.end ())
    associated_type_mappings.erase (it);
}

// lookup any associated type mappings, the out parameter of mapping is
// allowed to be nullptr which allows this interface to do a simple does exist
// check
bool
TypeCheckContext::lookup_associated_type_mapping (HirId id, HirId *mapping)
{
  auto it = associated_type_mappings.find (id);
  if (it == associated_type_mappings.end ())
    return false;

  if (mapping != nullptr)
    *mapping = it->second;

  return true;
}

void
TypeCheckContext::insert_associated_impl_mapping (
  HirId trait_id, const TyTy::BaseType *impl_type, HirId impl_id)
{
  auto it = associated_traits_to_impls.find (trait_id);
  if (it == associated_traits_to_impls.end ())
    {
      associated_traits_to_impls[trait_id] = {};
    }

  associated_traits_to_impls[trait_id].push_back ({impl_type, impl_id});
}

bool
TypeCheckContext::lookup_associated_impl_mapping_for_self (
  HirId trait_id, const TyTy::BaseType *self, HirId *mapping)
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

void
TypeCheckContext::insert_autoderef_mappings (
  HirId id, std::vector<Adjustment> &&adjustments)
{
  autoderef_mappings.emplace (id, std::move (adjustments));
}

bool
TypeCheckContext::lookup_autoderef_mappings (
  HirId id, std::vector<Adjustment> **adjustments)
{
  auto it = autoderef_mappings.find (id);
  if (it == autoderef_mappings.end ())
    return false;

  *adjustments = &it->second;
  return true;
}

void
TypeCheckContext::insert_cast_autoderef_mappings (
  HirId id, std::vector<Adjustment> &&adjustments)
{
  cast_autoderef_mappings.emplace (id, std::move (adjustments));
}

bool
TypeCheckContext::lookup_cast_autoderef_mappings (
  HirId id, std::vector<Adjustment> **adjustments)
{
  auto it = cast_autoderef_mappings.find (id);
  if (it == cast_autoderef_mappings.end ())
    return false;

  *adjustments = &it->second;
  return true;
}

void
TypeCheckContext::insert_variant_definition (HirId id, HirId variant)
{
  auto it = variants.find (id);
  rust_assert (it == variants.end ());

  variants[id] = variant;
}

bool
TypeCheckContext::lookup_variant_definition (HirId id, HirId *variant)
{
  auto it = variants.find (id);
  if (it == variants.end ())
    return false;

  *variant = it->second;
  return true;
}

void
TypeCheckContext::insert_operator_overload (HirId id, TyTy::FnType *call_site)
{
  auto it = operator_overloads.find (id);
  rust_assert (it == operator_overloads.end ());

  operator_overloads[id] = call_site;
}

bool
TypeCheckContext::lookup_operator_overload (HirId id, TyTy::FnType **call)
{
  auto it = operator_overloads.find (id);
  if (it == operator_overloads.end ())
    return false;

  *call = it->second;
  return true;
}

void
TypeCheckContext::insert_unconstrained_check_marker (HirId id, bool status)
{
  unconstrained[id] = status;
}

bool
TypeCheckContext::have_checked_for_unconstrained (HirId id, bool *result)
{
  auto it = unconstrained.find (id);
  bool found = it != unconstrained.end ();
  if (!found)
    return false;

  *result = it->second;
  return true;
}

void
TypeCheckContext::insert_resolved_predicate (HirId id,
					     TyTy::TypeBoundPredicate predicate)
{
  // auto it = predicates.find (id);
  // rust_assert (it == predicates.end ());

  predicates.insert ({id, predicate});
}

bool
TypeCheckContext::lookup_predicate (HirId id, TyTy::TypeBoundPredicate *result)
{
  auto it = predicates.find (id);
  bool found = it != predicates.end ();
  if (!found)
    return false;

  *result = it->second;
  return true;
}

void
TypeCheckContext::insert_query (HirId id)
{
  querys_in_progress.insert (id);
}

void
TypeCheckContext::query_completed (HirId id)
{
  querys_in_progress.erase (id);
}

bool
TypeCheckContext::query_in_progress (HirId id) const
{
  return querys_in_progress.find (id) != querys_in_progress.end ();
}

void
TypeCheckContext::insert_trait_query (DefId id)
{
  trait_queries_in_progress.insert (id);
}

void
TypeCheckContext::trait_query_completed (DefId id)
{
  trait_queries_in_progress.erase (id);
}

bool
TypeCheckContext::trait_query_in_progress (DefId id) const
{
  return trait_queries_in_progress.find (id)
	 != trait_queries_in_progress.end ();
}

Lifetime
TypeCheckContext::intern_lifetime (const HIR::Lifetime &lifetime)
{
  if (lifetime.get_lifetime_type () == AST::Lifetime::NAMED)
    {
      auto maybe_interned = lookup_lifetime (lifetime);
      if (maybe_interned)
	return *maybe_interned;

      auto interned = next_lifetime_index.next ();
      lifetime_name_interner[lifetime.get_name ()] = interned;
      return interned;
    }
  if (lifetime.get_lifetime_type () == AST::Lifetime::WILDCARD)
    {
      return next_lifetime_index.next ();
    }
  if (lifetime.get_lifetime_type () == AST::Lifetime::STATIC)
    {
      return Lifetime::static_lifetime ();
    }
  rust_unreachable ();
}

tl::optional<Lifetime>
TypeCheckContext::lookup_lifetime (const HIR::Lifetime &lifetime) const
{
  if (lifetime.get_lifetime_type () == AST::Lifetime::NAMED)
    {
      rust_assert (lifetime.get_name () != "static");
      const auto name = lifetime.get_name ();
      auto it = lifetime_name_interner.find (name);
      if (it == lifetime_name_interner.end ())
	return tl::nullopt;
      return it->second;
    }
  if (lifetime.get_lifetime_type () == AST::Lifetime::WILDCARD)
    {
      return Lifetime::anonymous_lifetime ();
    }
  if (lifetime.get_lifetime_type () == AST::Lifetime::STATIC)
    {
      return Lifetime::static_lifetime ();
    }
  rust_unreachable ();
}

WARN_UNUSED_RESULT tl::optional<TyTy::Region>
TypeCheckContext::lookup_and_resolve_lifetime (
  const HIR::Lifetime &lifetime) const
{
  auto maybe_interned = lookup_lifetime (lifetime);
  if (!maybe_interned)
    return tl::nullopt;

  return get_lifetime_resolver ().resolve (maybe_interned.value ());
}
void
TypeCheckContext::intern_and_insert_lifetime (const HIR::Lifetime &lifetime)
{
  get_lifetime_resolver ().insert_mapping (intern_lifetime (lifetime));
}

WARN_UNUSED_RESULT std::vector<TyTy::Region>
TypeCheckContext::regions_from_generic_args (const HIR::GenericArgs &args) const
{
  std::vector<TyTy::Region> regions;
  for (const auto &lifetime : args.get_lifetime_args ())
    {
      auto resolved = lookup_and_resolve_lifetime (lifetime);
      if (!resolved)
	{
	  rust_error_at (lifetime.get_locus (), "unresolved lifetime");
	  return {};
	}
      regions.push_back (*resolved);
    }
  return regions;
}

void
TypeCheckContext::compute_inference_variables (bool error)
{
  auto mappings = Analysis::Mappings::get ();

  // default inference variables if possible
  iterate ([&] (HirId id, TyTy::BaseType *ty) mutable -> bool {
    // nothing to do
    if (ty->get_kind () != TyTy::TypeKind::INFER)
      return true;

    TyTy::InferType *infer_var = static_cast<TyTy::InferType *> (ty);
    TyTy::BaseType *default_type;

    rust_debug_loc (mappings->lookup_location (id),
		    "trying to default infer-var: %s",
		    infer_var->as_string ().c_str ());
    bool ok = infer_var->default_type (&default_type);
    if (!ok)
      {
	if (error)
	  rust_error_at (mappings->lookup_location (id), ErrorCode::E0282,
			 "type annotations needed");
	return true;
      }

    auto result
      = unify_site (id, TyTy::TyWithLocation (ty),
		    TyTy::TyWithLocation (default_type), UNDEF_LOCATION);
    rust_assert (result);
    rust_assert (result->get_kind () != TyTy::TypeKind::ERROR);
    result->set_ref (id);
    insert_type (Analysis::NodeMapping (mappings->get_current_crate (), 0, id,
					UNKNOWN_LOCAL_DEFID),
		 result);

    return true;
  });
}

TyTy::VarianceAnalysis::CrateCtx &
TypeCheckContext::get_variance_analysis_ctx ()
{
  return variance_analysis_ctx;
}

// TypeCheckContextItem

TypeCheckContextItem::Item::Item (HIR::Function *item) : item (item) {}

TypeCheckContextItem::Item::Item (HIR::ImplBlock *impl_block,
				  HIR::Function *item)
  : impl_item ({impl_block, item})
{}

TypeCheckContextItem::Item::Item (HIR::TraitItemFunc *trait_item)
  : trait_item (trait_item)
{}

TypeCheckContextItem::TypeCheckContextItem (HIR::Function *item)
  : type (ItemType::ITEM), item (item)
{}

TypeCheckContextItem::TypeCheckContextItem (HIR::ImplBlock *impl_block,
					    HIR::Function *item)
  : type (ItemType::IMPL_ITEM), item (impl_block, item)
{}

TypeCheckContextItem::TypeCheckContextItem (HIR::TraitItemFunc *trait_item)
  : type (ItemType::TRAIT_ITEM), item (trait_item)
{}

TypeCheckContextItem::TypeCheckContextItem (const TypeCheckContextItem &other)
  : type (other.type), item (other.item)
{
  switch (other.type)
    {
    case ITEM:
      item.item = other.item.item;
      break;

    case IMPL_ITEM:
      item.impl_item = other.item.impl_item;
      break;

    case TRAIT_ITEM:
      item.trait_item = other.item.trait_item;
      break;

    case ERROR:
      item.item = nullptr;
      break;
    }
}

TypeCheckContextItem::TypeCheckContextItem ()
  : type (ItemType::ERROR), item (static_cast<HIR::Function *> (nullptr))
{}

TypeCheckContextItem &
TypeCheckContextItem::operator= (const TypeCheckContextItem &other)
{
  type = other.type;
  switch (other.type)
    {
    case ITEM:
      item.item = other.item.item;
      break;

    case IMPL_ITEM:
      item.impl_item = other.item.impl_item;
      break;

    case TRAIT_ITEM:
      item.trait_item = other.item.trait_item;
      break;

    case ERROR:
      item.item = nullptr;
      break;
    }

  return *this;
}

TypeCheckContextItem
TypeCheckContextItem::get_error ()
{
  return TypeCheckContextItem ();
}

bool
TypeCheckContextItem::is_error () const
{
  return type == ERROR;
}

HIR::Function *
TypeCheckContextItem::get_item ()
{
  rust_assert (get_type () == ItemType::ITEM);
  return item.item;
}

std::pair<HIR::ImplBlock *, HIR::Function *> &
TypeCheckContextItem::get_impl_item ()
{
  rust_assert (get_type () == ItemType::IMPL_ITEM);
  return item.impl_item;
}

HIR::TraitItemFunc *
TypeCheckContextItem::get_trait_item ()
{
  rust_assert (get_type () == ItemType::TRAIT_ITEM);
  return item.trait_item;
}

TypeCheckContextItem::ItemType
TypeCheckContextItem::get_type () const
{
  return type;
}

TyTy::FnType *
TypeCheckContextItem::get_context_type ()
{
  auto &context = *TypeCheckContext::get ();

  HirId reference = UNKNOWN_HIRID;
  switch (get_type ())
    {
    case ITEM:
      reference = get_item ()->get_mappings ().get_hirid ();
      break;

    case IMPL_ITEM:
      reference = get_impl_item ().second->get_mappings ().get_hirid ();
      break;

    case TRAIT_ITEM:
      reference = get_trait_item ()->get_mappings ().get_hirid ();
      break;

    case ERROR:
      rust_unreachable ();
      return nullptr;
    }

  rust_assert (reference != UNKNOWN_HIRID);

  TyTy::BaseType *lookup = nullptr;
  bool ok = context.lookup_type (reference, &lookup);
  rust_assert (ok);
  rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);
  return static_cast<TyTy::FnType *> (lookup);
}

DefId
TypeCheckContextItem::get_defid () const
{
  switch (get_type ())
    {
    case ITEM:
      return item.item->get_mappings ().get_defid ();

    case IMPL_ITEM:
      return item.impl_item.second->get_mappings ().get_defid ();

    case TRAIT_ITEM:
      return item.trait_item->get_mappings ().get_defid ();

    case ERROR:
      return UNKNOWN_DEFID;
    }

  return UNKNOWN_DEFID;
}

} // namespace Resolver
} // namespace Rust
