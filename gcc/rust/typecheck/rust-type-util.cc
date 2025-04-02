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

#include "rust-type-util.h"
#include "rust-diagnostics.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-item.h"
#include "rust-hir-type-check.h"
#include "rust-hir-type-check-type.h"
#include "rust-casts.h"
#include "rust-unify.h"
#include "rust-coercion.h"
#include "rust-hir-type-bounds.h"
#include "rust-immutable-name-resolution-context.h"
#include "options.h"

namespace Rust {
namespace Resolver {

bool
query_type (HirId reference, TyTy::BaseType **result)
{
  auto &mappings = Analysis::Mappings::get ();
  auto &resolver = *Resolver::get ();
  TypeCheckContext *context = TypeCheckContext::get ();

  if (context->query_in_progress (reference))
    return false;

  if (context->lookup_type (reference, result))
    return true;

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
      *result = TypeCheckItem::Resolve (*item.value ());
      context->query_completed (reference);
      return true;
    }

  if (auto impl_item = mappings.lookup_hir_implitem (reference))
    {
      auto impl_block
	= mappings.lookup_hir_impl_block (impl_item->second).value ();

      // found an impl item
      rust_debug_loc (impl_item->first->get_locus (),
		      "resolved impl-item {%u} to", reference);

      *result = TypeCheckItem::ResolveImplItem (*impl_block, *impl_item->first);
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

	  if (flag_name_resolution_2_0)
	    {
	      auto &nr_ctx = Resolver2_0::ImmutableNameResolutionContext::get ()
			       .resolver ();

	      // assign the ref_node_id if we've found something
	      nr_ctx.lookup (ast_node_id)
		.map (
		  [&ref_node_id] (NodeId resolved) { ref_node_id = resolved; });
	    }
	  else if (!resolver.lookup_resolved_name (ast_node_id, &ref_node_id))
	    resolver.lookup_resolved_type (ast_node_id, &ref_node_id);

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
		  location_t unify_locus, bool emit_errors)
{
  TyTy::BaseType *result
    = unify_site_and (UNKNOWN_HIRID, lhs, rhs, unify_locus, emit_errors,
		      false /*commit*/, true /*infer*/, true /*cleanup*/);
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
			      true /*emit_error*/, false /*infer*/, commits,
			      infers);
}

TyTy::BaseType *
unify_site_and (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
		location_t unify_locus, bool emit_errors, bool commit_if_ok,
		bool implicit_infer_vars, bool cleanup)
{
  TypeCheckContext &context = *TypeCheckContext::get ();

  TyTy::BaseType *expected = lhs.get_ty ();
  TyTy::BaseType *expr = rhs.get_ty ();

  rust_debug (
    "unify_site_and commit %s infer %s id={%u} expected={%s} expr={%s}",
    commit_if_ok ? "true" : "false", implicit_infer_vars ? "true" : "false", id,
    expected->debug_str ().c_str (), expr->debug_str ().c_str ());

  std::vector<UnifyRules::CommitSite> commits;
  std::vector<UnifyRules::InferenceSite> infers;
  TyTy::BaseType *result
    = UnifyRules::Resolve (lhs, rhs, unify_locus, false /*commit inline*/,
			   emit_errors, implicit_infer_vars, commits, infers);
  bool ok = result->get_kind () != TyTy::TypeKind::ERROR;
  if (ok && commit_if_ok)
    {
      for (auto &c : commits)
	{
	  UnifyRules::commit (c.lhs, c.rhs, c.resolved);
	}
    }
  else if (cleanup)
    {
      // FIXME
      // reset the get_next_hir_id

      for (auto &i : infers)
	{
	  i.param->set_ref (i.pref);
	  i.param->set_ty_ref (i.ptyref);

	  // remove the inference variable
	  context.clear_type (i.infer);
	  delete i.infer;
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
			      const TyTy::BaseType *binding, bool *ambigious)
{
  auto context = TypeCheckContext::get ();

  // setup any associated type mappings for the specified bonds and this
  // type
  auto candidates = TypeBoundsProbe::Probe (binding);
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
	  if (bound.is_equal (associated->get_predicate ()))
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

} // namespace Resolver
} // namespace Rust
