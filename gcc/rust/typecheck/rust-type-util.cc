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

#include "rust-hir-item.h"
#include "rust-system.h"
#include "rust-type-util.h"
#include "rust-diagnostics.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-item.h"
#include "rust-hir-type-check.h"
#include "rust-casts.h"
#include "rust-mapping-common.h"
#include "rust-rib.h"
#include "rust-unify.h"
#include "rust-coercion.h"
#include "rust-hir-type-bounds.h"
#include "rust-hir-trait-resolve.h"
#include "rust-substitution-mapper.h"
#include "rust-finalized-name-resolution-context.h"

namespace Rust {
namespace Resolver {

const size_t kNormalizeProjectionLimit = 10;

bool
query_type (HirId reference, TyTy::BaseType **result)
{
  auto &mappings = Analysis::Mappings::get ();
  TypeCheckContext *context = TypeCheckContext::get ();

  if (context->lookup_type (reference, result))
    return true;

  if (context->query_in_progress (reference))
    return false;

  context->insert_query (reference);

  std::pair<HIR::Enum *, HIR::EnumItem *> enum_candidiate
    = mappings.lookup_hir_enumitem (reference);
  bool enum_candidiate_ok
    = enum_candidiate.first != nullptr && enum_candidiate.second != nullptr;
  if (enum_candidiate_ok)
    {
      HIR::Enum *parent = enum_candidiate.first;
      HIR::EnumItem *enum_item = enum_candidiate.second;
      rust_debug_loc (enum_item->get_locus (), "resolved item {%u} to",
		      reference);

      *result = TypeCheckItem::Resolve (*parent);

      context->query_completed (reference);
      return true;
    }

  if (auto item = mappings.lookup_hir_item (reference))
    {
      rust_debug_loc (item.value ()->get_locus (), "resolved item {%u} to",
		      reference);

      DefId item_defid = item.value ()->get_mappings ().get_defid ();
      bool is_local = item_defid.crateNum == mappings.get_current_crate ();
      bool is_fn
	= item.value ()->get_item_kind () == HIR::Item::ItemKind::Function;
      if (is_fn && is_local)
	{
	  HIR::Function &fn = *static_cast<HIR::Function *> (item.value ());
	  *result = TypeCheckItem::ResolveFunctionSignature (fn);
	}
      else if (item.value ()->get_item_kind () == HIR::Item::ItemKind::Trait
	       && is_local)
	{
	  HIR::Trait &trait = *static_cast<HIR::Trait *> (item.value ());
	  *result = TypeCheckItem::ResolveTraitSignature (trait);
	}
      else
	{
	  *result = TypeCheckItem::Resolve (*item.value ());
	}

      context->query_completed (reference);
      return true;
    }

  if (auto impl_item = mappings.lookup_hir_implitem (reference))
    {
      auto impl_block
	= mappings.lookup_hir_impl_block (impl_item->second).value ();
      auto lifetime_pin = context->push_clean_lifetime_resolver (true);

      bool failure_flag = false;
      auto substitutions
	= TypeCheckItem::ResolveImplBlockSubstitutions (*impl_block,
							failure_flag);
      if (failure_flag)
	{
	  *result
	    = TypeCheckItem::ResolveImplItem (*impl_block, *impl_item->first);
	  context->query_completed (reference);
	  return true;
	}

      TyTy::BaseType *self = nullptr;
      bool ok
	= query_type (impl_block->get_type ().get_mappings ().get_hirid (),
		      &self);
      if (!ok)
	{
	  context->query_completed (reference);
	  return false;
	}

      tl::optional<ImplTraitFrameGuard> guard;
      if (impl_block->has_trait_ref ())
	{
	  HIR::TypePath &ref = impl_block->get_trait_ref ();
	  auto trait_reference = TraitResolver::Resolve (ref);
	  if (trait_reference->is_error ())
	    {
	      context->query_completed (reference);
	      return false;
	    }

	  auto specified_bound = TypeCheckBase::ResolvePredicateFromBound (
	    ref, impl_block->get_type (), impl_block->get_polarity ());

	  std::map<DefId, AssocTypeEntry> assoc_types_by_trait_item;
	  std::vector<const TraitItemReference *> trait_item_refs;
	  TypeCheckItem::ResolveImplTraitAssociatedTypes (
	    context, *impl_block, specified_bound, self, substitutions,
	    assoc_types_by_trait_item, trait_item_refs);

	  ImplTraitContextFrame frame{trait_reference, self,
				      std::move (assoc_types_by_trait_item)};
	  guard.emplace (frame);
	}

      // found an impl item
      rust_debug_loc (impl_item->first->get_locus (),
		      "resolved impl-item {%u} to", reference);

      DefId item_defid = impl_item->first->get_impl_mappings ().get_defid ();
      bool is_local = item_defid.crateNum == mappings.get_current_crate ();
      if (impl_item->first->get_impl_item_type () == HIR::ImplItem::FUNCTION
	  && is_local)
	{
	  HIR::Function &fn = *static_cast<HIR::Function *> (impl_item->first);
	  *result = TypeCheckImplItem::ResolveFunctionSignature (
	    *impl_block, fn, self, std::move (substitutions));
	}
      else
	*result = TypeCheckImplItem::Resolve (*impl_block, *impl_item->first,
					      self, std::move (substitutions));

      context->query_completed (reference);
      return true;
    }

  // is it an impl_type?
  if (auto impl_block_by_type = mappings.lookup_impl_block_type (reference))
    {
      // found an impl item
      HIR::ImplBlock *impl = impl_block_by_type.value ();
      rust_debug_loc (impl->get_locus (), "resolved impl block type {%u} to",
		      reference);

      // this could be recursive to the root type
      if (impl->has_type ())
	{
	  HIR::Type &ty = impl->get_type ();
	  NodeId ref_node_id = UNKNOWN_NODEID;
	  NodeId ast_node_id = ty.get_mappings ().get_nodeid ();

	  auto &nr_ctx = Resolver2_0::FinalizedNameResolutionContext::get ();

	  // assign the ref_node_id if we've found something
	  nr_ctx.lookup (ast_node_id, Resolver2_0::Namespace::Types)
	    .map ([&ref_node_id] (NodeId resolved) { ref_node_id = resolved; });

	  if (ref_node_id != UNKNOWN_NODEID)
	    {
	      tl::optional<HirId> hid
		= mappings.lookup_node_to_hir (ref_node_id);
	      if (hid.has_value () && context->query_in_progress (hid.value ()))
		{
		  context->query_completed (reference);
		  return false;
		}
	    }
	}

      *result = TypeCheckItem::ResolveImplBlockSelf (*impl);
      context->query_completed (reference);
      return true;
    }

  // is it an extern item?
  if (auto extern_item = mappings.lookup_hir_extern_item (reference))
    {
      auto block = mappings.lookup_hir_extern_block (extern_item->second);
      rust_assert (block.has_value ());

      *result
	= TypeCheckTopLevelExternItem::Resolve (*extern_item.value ().first,
						*block.value ());
      context->query_completed (reference);
      return true;
    }

  // more?
  location_t possible_locus = mappings.lookup_location (reference);
  rust_debug_loc (possible_locus, "query system failed to resolve: [%u]",
		  reference);
  context->query_completed (reference);

  return false;
}

bool
types_compatable (TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
		  location_t unify_locus, bool emit_errors, bool check_bounds)
{
  TyTy::BaseType *result
    = unify_site_and (UNKNOWN_HIRID, lhs, rhs, unify_locus, emit_errors,
		      false /*commit*/, true /*infer*/, true /*cleanup*/,
		      check_bounds);
  return result->get_kind () != TyTy::TypeKind::ERROR;
}

TyTy::BaseType *
unify_site (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
	    location_t unify_locus)
{
  TyTy::BaseType *expected = lhs.get_ty ();
  TyTy::BaseType *expr = rhs.get_ty ();

  rust_debug ("unify_site id={%u} expected={%s} expr={%s}", id,
	      expected->debug_str ().c_str (), expr->debug_str ().c_str ());

  std::vector<UnifyRules::CommitSite> commits;
  std::vector<UnifyRules::InferenceSite> infers;
  return UnifyRules::Resolve (lhs, rhs, unify_locus, true /*commit*/,
			      true /*emit_error*/, false /*infer*/,
			      true /*check_bounds*/, commits, infers);
}

TyTy::BaseType *
unify_site_and (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
		location_t unify_locus, bool emit_errors, bool commit_if_ok,
		bool implicit_infer_vars, bool cleanup, bool check_bounds)
{
  TypeCheckContext &context = *TypeCheckContext::get ();

  TyTy::BaseType *expected = lhs.get_ty ();
  TyTy::BaseType *expr = rhs.get_ty ();

  rust_debug_loc (unify_locus,
		  "begin unify_site_and commit %s infer %s check_bounds %s "
		  "id={%u} expected={%s} expr={%s}",
		  commit_if_ok ? "true" : "false",
		  implicit_infer_vars ? "true" : "false",
		  check_bounds ? "true" : "false", id == UNKNOWN_HIRID ? 0 : id,
		  expected->debug_str ().c_str (), expr->debug_str ().c_str ());

  std::vector<UnifyRules::CommitSite> commits;
  std::vector<UnifyRules::InferenceSite> infers;
  TyTy::BaseType *result
    = UnifyRules::Resolve (lhs, rhs, unify_locus, false /*commit inline*/,
			   emit_errors, implicit_infer_vars, check_bounds,
			   commits, infers);
  bool ok = result->get_kind () != TyTy::TypeKind::ERROR;

  rust_debug_loc (unify_locus,
		  "unify_site_and done ok=%s commit %s infer %s id={%u} "
		  "expected={%s} expr={%s}",
		  ok ? "true" : "false", commit_if_ok ? "true" : "false",
		  implicit_infer_vars ? "true" : "false",
		  id == UNKNOWN_HIRID ? 0 : id, expected->debug_str ().c_str (),
		  expr->debug_str ().c_str ());

  if (ok && commit_if_ok)
    {
      for (auto &c : commits)
	{
	  UnifyRules::commit (c.lhs, c.rhs, c.resolved);
	}
    }
  else if (cleanup)
    {
      for (auto &i : infers)
	{
	  if (i.param != nullptr)
	    {
	      i.param->set_ref (i.pref);
	      i.param->set_ty_ref (i.ptyref);
	    }

	  // remove the inference variable
	  context.clear_type (i.infer);
	  // FIXME: Don't delete - result might point to this
	  // delete i.infer;
	}
    }
  return result;
}

TyTy::BaseType *
coercion_site (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
	       location_t locus)
{
  TyTy::BaseType *expected = lhs.get_ty ();
  TyTy::BaseType *expr = rhs.get_ty ();

  rust_debug ("coercion_site id={%u} expected={%s} expr={%s}", id,
	      expected->debug_str ().c_str (), expr->debug_str ().c_str ());

  auto context = TypeCheckContext::get ();
  if (expected->get_kind () == TyTy::TypeKind::ERROR
      || expr->get_kind () == TyTy::TypeKind::ERROR)
    return expr;

  // can we autoderef it?
  auto result = TypeCoercionRules::Coerce (expr, expected, locus,
					   true /*allow-autodref*/);

  // the result needs to be unified
  TyTy::BaseType *receiver = expr;
  if (!result.is_error ())
    {
      receiver = result.tyty;
    }

  rust_debug ("coerce_default_unify(a={%s}, b={%s})",
	      receiver->debug_str ().c_str (), expected->debug_str ().c_str ());
  TyTy::BaseType *coerced
    = unify_site_and (id, lhs,
		      TyTy::TyWithLocation (receiver, rhs.get_locus ()), locus,
		      true /*emit_error*/, true /*commit*/, true /*infer*/,
		      true /*cleanup*/);
  context->insert_autoderef_mappings (id, std::move (result.adjustments));
  return coerced;
}

TyTy::BaseType *
try_coercion (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
	      location_t locus)
{
  TyTy::BaseType *expected = lhs.get_ty ();
  TyTy::BaseType *expr = rhs.get_ty ();

  rust_debug ("try_coercion_site id={%u} expected={%s} expr={%s}", id,
	      expected->debug_str ().c_str (), expr->debug_str ().c_str ());

  auto result = TypeCoercionRules::TryCoerce (expr, expected, locus,
					      true /*allow-autodref*/);
  if (result.is_error ())
    return new TyTy::ErrorType (id);

  return result.tyty;
}

TyTy::BaseType *
cast_site (HirId id, TyTy::TyWithLocation from, TyTy::TyWithLocation to,
	   location_t cast_locus)
{
  rust_debug ("cast_site id={%u} from={%s} to={%s}", id,
	      from.get_ty ()->debug_str ().c_str (),
	      to.get_ty ()->debug_str ().c_str ());

  auto context = TypeCheckContext::get ();
  if (from.get_ty ()->get_kind () == TyTy::TypeKind::ERROR
      || to.get_ty ()->get_kind () == TyTy::TypeKind::ERROR)
    return to.get_ty ();

  // do the cast
  auto result = TypeCastRules::resolve (cast_locus, from, to);

  // we assume error has already been emitted
  if (result.is_error ())
    return to.get_ty ();

  // the result needs to be unified
  TyTy::BaseType *casted_result = result.tyty;
  rust_debug ("cast_default_unify(a={%s}, b={%s})",
	      casted_result->debug_str ().c_str (),
	      to.get_ty ()->debug_str ().c_str ());

  TyTy::BaseType *casted
    = unify_site (id, to,
		  TyTy::TyWithLocation (casted_result, from.get_locus ()),
		  cast_locus);
  context->insert_cast_autoderef_mappings (id, std::move (result.adjustments));
  return casted;
}

AssociatedImplTrait *
lookup_associated_impl_block (const TyTy::TypeBoundPredicate &bound,
			      TyTy::BaseType *binding, bool *ambigious)
{
  auto context = TypeCheckContext::get ();

  // setup any associated type mappings for the specified bonds and this
  // type
  auto candidates
    = TypeBoundsProbe::Probe (binding, bound.get ()->get_hir_trait_ref ());
  std::vector<AssociatedImplTrait *> associated_impl_traits;
  for (auto &probed_bound : candidates)
    {
      HIR::ImplBlock *associated_impl = probed_bound.second;

      HirId impl_block_id = associated_impl->get_mappings ().get_hirid ();
      AssociatedImplTrait *associated = nullptr;
      bool found_impl_trait
	= context->lookup_associated_trait_impl (impl_block_id, &associated);
      if (found_impl_trait)
	{
	  // compare the bounds from here i think is what we can do:
	  if (bound.get ()->is_equal (*associated->get_predicate ().get ()))
	    {
	      associated_impl_traits.push_back (associated);
	    }
	}
    }

  if (associated_impl_traits.empty ())
    return nullptr;

  // This code is important when you look at slices for example when
  // you have a slice such as:
  //
  // let slice = &array[1..3]
  //
  // the higher ranked bounds will end up having an Index trait
  // implementation for Range<usize> so we need this code to resolve
  // that we have an integer inference variable that needs to become
  // a usize
  //
  // The other complicated issue is that we might have an intrinsic
  // which requires the :Clone or Copy bound but the libcore adds
  // implementations for all the integral types so when there are
  // multiple candidates we need to resolve to the default
  // implementation for that type otherwise its an error for
  // ambiguous type bounds

  // if we have a non-general inference variable we need to be
  // careful about the selection here
  bool is_infer_var = binding->get_kind () == TyTy::TypeKind::INFER;
  bool is_integer_infervar
    = is_infer_var
      && static_cast<const TyTy::InferType *> (binding)->get_infer_kind ()
	   == TyTy::InferType::InferTypeKind::INTEGRAL;
  bool is_float_infervar
    = is_infer_var
      && static_cast<const TyTy::InferType *> (binding)->get_infer_kind ()
	   == TyTy::InferType::InferTypeKind::FLOAT;

  AssociatedImplTrait *associate_impl_trait = nullptr;
  if (associated_impl_traits.size () == 1)
    {
      // just go for it
      associate_impl_trait = associated_impl_traits.at (0);
    }
  else if (is_integer_infervar)
    {
      TyTy::BaseType *type = nullptr;
      bool ok = context->lookup_builtin ("i32", &type);
      rust_assert (ok);

      for (auto &impl : associated_impl_traits)
	{
	  bool found = impl->get_self ()->is_equal (*type);
	  if (found)
	    {
	      associate_impl_trait = impl;
	      break;
	    }
	}
    }
  else if (is_float_infervar)
    {
      TyTy::BaseType *type = nullptr;
      bool ok = context->lookup_builtin ("f64", &type);
      rust_assert (ok);

      for (auto &impl : associated_impl_traits)
	{
	  bool found = impl->get_self ()->is_equal (*type);
	  if (found)
	    {
	      associate_impl_trait = impl;
	      break;
	    }
	}
    }

  if (associate_impl_trait == nullptr && ambigious != nullptr)
    {
      *ambigious = true;
    }

  return associate_impl_trait;
}

void
rebind_projection_self_from_fn (TyTy::FnType &fn, TyTy::BaseType *root)
{
  // After substitution+monomorphize the fn's substitution clones carry the
  // call-site bindings. Inner projections in the return/params can still
  // reference the traits formal Self via TyVar(formal.ref) whose type table
  // entry resolves to the unbound formal
  std::map<HirId, TyTy::BaseGeneric *> bound_by_formal;
  for (auto &sub : fn.get_substs ())
    {
      auto *pty = sub.get_param_ty ();
      if (pty == nullptr || pty->get_kind () != TyTy::TypeKind::PARAM
	  || !pty->can_resolve ())
	continue;
      bound_by_formal[pty->get_ref ()] = pty;
    }

  std::function<void (TyTy::BaseType *)> rebind;
  rebind = [&] (TyTy::BaseType *ty) {
    if (ty == nullptr)
      return;

    if (auto *proj = ty->try_as<TyTy::ProjectionType> ())
      {
	auto *self = proj->get_self ();
	if (self->get_kind () == TyTy::TypeKind::PARAM)
	  {
	    auto it = bound_by_formal.find (self->get_ref ());
	    if (it != bound_by_formal.end ())
	      proj->set_self (it->second);
	  }

	for (auto &sub : proj->get_substs ())
	  {
	    auto *param = sub.get_param_ty ();
	    if (param == nullptr || param->get_kind () != TyTy::TypeKind::PARAM
		|| param->can_resolve ())
	      continue;

	    auto it = bound_by_formal.find (param->get_ref ());
	    if (it == bound_by_formal.end ()
		|| it->second->get_kind () != TyTy::TypeKind::PARAM)
	      continue;

	    auto *bound = static_cast<TyTy::ParamType *> (it->second);
	    if (bound->can_resolve ())
	      param->set_ty_ref (bound->get_ty_ref ());
	  }
	return;
      }

    if (auto *adt = ty->try_as<TyTy::ADTType> ())
      {
	for (auto &variant : adt->get_variants ())
	  for (auto &field : variant->get_fields ())
	    rebind (field->get_field_type ());
	return;
      }

    if (auto *ref = ty->try_as<TyTy::ReferenceType> ())
      rebind (ref->get_base ());
    else if (auto *ptr = ty->try_as<TyTy::PointerType> ())
      rebind (ptr->get_base ());
    else if (auto *tup = ty->try_as<TyTy::TupleType> ())
      for (size_t i = 0; i < tup->num_fields (); i++)
	rebind (tup->get_field (i));
  };

  rebind (root);
}

TyTy::BaseType *
normalize_projection (TyTy::ProjectionType *proj, location_t locus,
		      bool emit_errors, bool unify_self)
{
  static std::vector<TyTy::ProjectionType *> active_projections;
  if (ScopedPush<TyTy::ProjectionType *>::contains (active_projections, proj))
    return proj;

  ScopedPush<TyTy::ProjectionType *> guard (active_projections, proj);

  if (!proj->is_trait_position ())
    {
      TyTy::BaseType *base = proj->get ();
      if (auto *param = base->try_as<TyTy::ParamType> ())
	{
	  if (param->can_resolve ())
	    {
	      TyTy::BaseType *resolved
		= TyTy::TyVar (param->get_ty_ref ()).get_tyty ();
	      if (!resolved->is<TyTy::ParamType> ())
		base = resolved;
	    }
	}
      if (auto *base_proj = base->try_as<TyTy::ProjectionType> ())
	return normalize_projection (base_proj, locus, emit_errors, unify_self);
      return base;
    }

  // special case the discriminant_type lang item
  auto &mappings = Analysis::Mappings::get ();
  auto *ctx = TypeCheckContext::get ();
  if (auto discriminant_type_id
      = mappings.lookup_lang_item (LangItem::Kind::DISCRIMINANT_TYPE))
    {
      if (proj->get_item_defid () == discriminant_type_id.value ())
	{
	  TyTy::BaseType *isize_ty = nullptr;
	  bool ok = ctx->lookup_builtin ("isize", &isize_ty);
	  rust_assert (ok);

	  // If the self type is a concrete ADT, use its repr.
	  TyTy::BaseType *self = proj->get_self ()->destructure ();
	  if (auto *adt = self->try_as<TyTy::ADTType> ())
	    {
	      auto *repr = adt->get_repr_options ().repr;
	      if (repr != nullptr)
		return repr;
	    }
	  return isize_ty;
	}
    }

  ImplTraitContextFrame frame;
  if (!ctx->find_matching_impl_trait_frame (*proj->get_trait_ref (),
					    *proj->get_self (), &frame))
    {
      // No concrete impl frame check WHERE clause bindings on the self type
      //
      //   fn foo<T: Trait<AssocType = X>>()
      //
      // normalizes
      //
      //    <T as Trait>::AssocType -> X.

      TyTy::BaseType *self = proj->get_self ()->destructure ();
      const DefId item_defid = proj->get_item_defid ();

      // find the name of the associated type from the trait reference
      std::string assoc_name;
      for (const auto &ti : proj->get_trait_ref ()->get_trait_items ())
	{
	  if (ti.get_mappings ().get_defid () == item_defid)
	    {
	      assoc_name = ti.get_identifier ();
	      break;
	    }
	}

      if (!assoc_name.empty ())
	{
	  for (auto &bound : self->get_specified_bounds ())
	    {
	      if (!bound.get ()->is_equal (*proj->get_trait_ref ()))
		continue;

	      auto &binding
		= bound.get_substitution_arguments ().get_binding_args ();
	      auto it = binding.find (assoc_name);
	      if (it != binding.end ())
		return it->second;
	    }
	}

      // If self is a trait-position projection recursively normalize it first
      // so the impl-block lookup below works on a concrete type.
      if (auto *self_proj = self->try_as<TyTy::ProjectionType> ())
	{
	  if (self_proj->is_trait_position ())
	    {
	      auto *norm = normalize_projection (self_proj, locus, emit_errors,
						 unify_self);
	      if (norm != self_proj
		  && norm->get_kind () != TyTy::TypeKind::ERROR)
		self = norm->destructure ();
	    }
	  else
	    {
	      auto *base = self_proj->get ();
	      if (base && base != self_proj)
		self = base->destructure ();
	    }
	}

      // Direct impl-block lookup for concrete self types (no active frame).
      if (!assoc_name.empty () && self->get_kind () != TyTy::TypeKind::PARAM
	  && self->get_kind () != TyTy::TypeKind::INFER
	  && self->get_kind () != TyTy::TypeKind::PROJECTION)
	{
	  auto candidates = TypeBoundsProbe::Probe (
	    self, proj->get_trait_ref ()->get_hir_trait_ref ());
	  for (auto &probed : candidates)
	    {
	      HIR::ImplBlock *impl_block = probed.second;
	      if (!impl_block->has_trait_ref ())
		continue;

	      HIR::TypePath &ref = impl_block->get_trait_ref ();
	      auto *tref = TraitResolver::Resolve (ref);
	      if (tref->is_error ()
		  || !tref->is_equal (*proj->get_trait_ref ()))
		continue;

	      for (auto &impl_item : impl_block->get_impl_items ())
		{
		  if (impl_item->get_impl_item_name ().compare (assoc_name)
		      != 0)
		    continue;

		  TyTy::BaseType *result = nullptr;
		  if (query_type (impl_item->get_impl_mappings ().get_hirid (),
				  &result))
		    {
		      AssociatedImplTrait *associated = nullptr;
		      if (ctx->lookup_associated_trait_impl (
			    impl_block->get_mappings ().get_hirid (),
			    &associated)
			  && associated != nullptr)
			{
			  auto mapping
			    = associated->bind_impl_for_projection (*proj,
								    locus);
			  if (!mapping.is_error ())
			    result
			      = SubstMapperInternal::Resolve (result, mapping);
			}

		      // impl type aliases are stored as non-trait-position
		      // ProjectionType; unwrap to base only when there are no
		      // substitution params (non-GAT)
		      if (auto *p = result->try_as<TyTy::ProjectionType> ())
			{
			  if (!p->is_trait_position ())
			    {
			      bool all_substs_bound = true;
			      for (auto &s : p->get_substs ())
				{
				  auto *sp = s.get_param_ty ();
				  if (sp == nullptr || !sp->can_resolve ()
				      || sp->resolve ()->get_kind ()
					   == TyTy::TypeKind::PARAM)
				    {
				      all_substs_bound = false;
				      break;
				    }
				}

			      // Also unwrap when the base is already concrete
			      bool base_is_concrete
				= p->get () != nullptr
				  && p->get ()->is_concrete ();
			      if (!p->has_substitutions () || all_substs_bound
				  || base_is_concrete)
				result = p->get ();
			    }
			}

		      return result;
		    }
		  break;
		}
	    }
	}

      // special-case FnOnce::Output
      if (proj->is_trait_position ())
	{
	  auto fn_once_lookup
	    = mappings.lookup_lang_item (LangItem::Kind::FN_ONCE);
	  auto fn_once_output_lookup
	    = mappings.lookup_lang_item (LangItem::Kind::FN_ONCE_OUTPUT);
	  if (!fn_once_lookup || !fn_once_output_lookup)
	    return proj;

	  DefId &fn_once_trait_id = fn_once_lookup.value ();
	  DefId &fn_once_output_id = fn_once_output_lookup.value ();
	  DefId proj_trait_id = proj->get_trait_ref ()->get_defid ();
	  DefId proj_trait_item_id = proj->get_item_defid ();

	  if (proj_trait_id == fn_once_trait_id
	      && proj_trait_item_id == fn_once_output_id)
	    {
	      auto pself = proj->get_self ();
	      if (auto closure = pself->try_as<TyTy::ClosureType> ())
		return &closure->get_result_type ();
	    }
	}

      return proj;
    }

  if (unify_self)
    {
      TyTy::BaseType *proj_self = proj->get_self ();
      TyTy::BaseType *impl_self = frame.self;
      TyTy::BaseType *self
	= unify_site_and (/*id*/ 0, TyTy::TyWithLocation (proj_self, locus),
			  TyTy::TyWithLocation (impl_self, locus), locus,
			  /*emit_errors*/ false,
			  /*commit*/ false,
			  /*infer*/ false,
			  /*cleanup*/ true,
			  /*check_bounds*/ false);

      if (self->get_kind () == TyTy::TypeKind::ERROR)
	return self;
    }

  // Lookup the trait item -> impl type mapping (key = trait item DefId).
  const DefId item = proj->get_item_defid ();
  auto it = frame.assoc_types_by_trait_item.find (item);
  if (it == frame.assoc_types_by_trait_item.end ())
    {
      return proj;
    }

  auto &entry = it->second;
  auto impl_value = entry.value;

  // chase the impl body through any further projections it contains
  TyTy::BaseType *normalized = impl_value;
  for (size_t i = 0; i < kNormalizeProjectionLimit; i++)
    {
      if (!normalized->is<TyTy::ProjectionType> ())
	break;

      auto p = normalized->as<TyTy::ProjectionType> ();
      if (p->is_trait_position ())
	{
	  auto *n = normalize_projection (p, locus, emit_errors, unify_self);
	  if (n == p)
	    break;

	  normalized = n;
	}
      else
	{
	  auto *v = p->get ();
	  if (v == nullptr || v == p)
	    break;

	  normalized = v;
	}
    }

  return normalized;
}

} // namespace Resolver
} // namespace Rust
