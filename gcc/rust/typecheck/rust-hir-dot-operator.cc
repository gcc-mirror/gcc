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

#include "rust-hir-dot-operator.h"
#include "rust-hir-path-probe.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir-type-check-item.h"
#include "rust-type-util.h"
#include "rust-coercion.h"

namespace Rust {
namespace Resolver {

MethodResolver::MethodResolver (bool autoderef_flag,
				const HIR::PathIdentSegment &segment_name)
  : AutoderefCycle (autoderef_flag), segment_name (segment_name), result ()
{}

std::set<MethodCandidate>
MethodResolver::Probe (TyTy::BaseType *receiver,
		       const HIR::PathIdentSegment &segment_name,
		       bool autoderef_flag)
{
  MethodResolver resolver (autoderef_flag, segment_name);
  resolver.cycle (receiver);
  return resolver.result;
}

std::set<MethodCandidate>
MethodResolver::Select (std::set<MethodCandidate> &candidates,
			TyTy::BaseType *receiver,
			std::vector<TyTy::BaseType *> arguments)
{
  std::set<MethodCandidate> selected;
  for (auto &candidate : candidates)
    {
      TyTy::BaseType *candidate_type = candidate.candidate.ty;
      rust_assert (candidate_type->get_kind () == TyTy::TypeKind::FNDEF);
      TyTy::FnType &fn = *static_cast<TyTy::FnType *> (candidate_type);

      // match the number of arguments
      if (fn.num_params () != (arguments.size () + 1))
	continue;

      // match the arguments
      bool failed = false;
      for (size_t i = 0; i < arguments.size (); i++)
	{
	  TyTy::BaseType *arg = arguments.at (i);
	  TyTy::BaseType *param = fn.get_params ().at (i + 1).second;
	  TyTy::BaseType *coerced
	    = try_coercion (0, TyTy::TyWithLocation (param),
			    TyTy::TyWithLocation (arg), UNDEF_LOCATION);
	  if (coerced->get_kind () == TyTy::TypeKind::ERROR)
	    {
	      failed = true;
	      break;
	    }
	}

      if (!failed)
	selected.insert (candidate);
    }

  return selected;
}

void
MethodResolver::try_hook (const TyTy::BaseType &r)
{
  rust_debug ("MethodResolver::try_hook get_predicate_items: [%s]",
	      r.debug_str ().c_str ());
  const auto &specified_bounds = r.get_specified_bounds ();
  predicate_items = get_predicate_items (segment_name, r, specified_bounds);

  if (predicate_items.size () > 0)
    return;

  if (r.get_kind () == TyTy::TypeKind::REF)
    {
      const auto &ref = static_cast<const TyTy::ReferenceType &> (r);
      const auto &element = ref.get_var_element_type ();
      const auto &element_ty = *element.get_tyty ();
      const auto &specified_bounds = element_ty.get_specified_bounds ();
      predicate_items
	= get_predicate_items (segment_name, element_ty, specified_bounds);
    }
}

bool
MethodResolver::select (TyTy::BaseType &receiver)
{
  rust_debug ("MethodResolver::select reciever=[%s] path=[%s]",
	      receiver.debug_str ().c_str (),
	      segment_name.as_string ().c_str ());

  struct impl_item_candidate
  {
    HIR::Function *item;
    HIR::ImplBlock *impl_block;
    TyTy::FnType *ty;
  };

  const TyTy::BaseType *raw = receiver.destructure ();
  bool receiver_is_raw_ptr = raw->get_kind () == TyTy::TypeKind::POINTER;
  bool receiver_is_ref = raw->get_kind () == TyTy::TypeKind::REF;

  // assemble inherent impl items
  std::vector<impl_item_candidate> inherent_impl_fns;
  mappings->iterate_impl_items (
    [&] (HirId id, HIR::ImplItem *item, HIR::ImplBlock *impl) mutable -> bool {
      bool is_trait_impl = impl->has_trait_ref ();
      if (is_trait_impl)
	return true;

      bool is_fn
	= item->get_impl_item_type () == HIR::ImplItem::ImplItemType::FUNCTION;
      if (!is_fn)
	return true;

      HIR::Function *func = static_cast<HIR::Function *> (item);
      if (!func->is_method ())
	return true;

      bool name_matches = func->get_function_name ().as_string ().compare (
			    segment_name.as_string ())
			  == 0;
      if (!name_matches)
	return true;

      TyTy::BaseType *ty = nullptr;
      if (!query_type (func->get_mappings ().get_hirid (), &ty))
	return true;
      rust_assert (ty != nullptr);
      if (ty->get_kind () == TyTy::TypeKind::ERROR)
	return true;

      rust_assert (ty->get_kind () == TyTy::TypeKind::FNDEF);
      TyTy::FnType *fnty = static_cast<TyTy::FnType *> (ty);
      const TyTy::BaseType *impl_self
	= TypeCheckItem::ResolveImplBlockSelf (*impl);

      // see:
      // https://gcc-rust.zulipchat.com/#narrow/stream/266897-general/topic/Method.20Resolution/near/338646280
      // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/method/probe.rs#L650-L660
      bool impl_self_is_ptr = impl_self->get_kind () == TyTy::TypeKind::POINTER;
      bool impl_self_is_ref = impl_self->get_kind () == TyTy::TypeKind::REF;
      if (receiver_is_raw_ptr && impl_self_is_ptr)
	{
	  const TyTy::PointerType &sptr
	    = *static_cast<const TyTy::PointerType *> (impl_self);
	  const TyTy::PointerType &ptr
	    = *static_cast<const TyTy::PointerType *> (raw);

	  // we could do this via lang-item assemblies if we refactor this
	  bool mut_match = sptr.mutability () == ptr.mutability ();
	  if (!mut_match)
	    return true;
	}
      else if (receiver_is_ref && impl_self_is_ref)
	{
	  const TyTy::ReferenceType &sptr
	    = *static_cast<const TyTy::ReferenceType *> (impl_self);
	  const TyTy::ReferenceType &ptr
	    = *static_cast<const TyTy::ReferenceType *> (raw);

	  // we could do this via lang-item assemblies if we refactor this
	  bool mut_match = sptr.mutability () == ptr.mutability ();
	  if (!mut_match)
	    return true;
	}

      inherent_impl_fns.push_back ({func, impl, fnty});

      return true;
    });

  struct trait_item_candidate
  {
    const HIR::TraitItemFunc *item;
    const HIR::Trait *trait;
    TyTy::FnType *ty;
    const TraitReference *reference;
    const TraitItemReference *item_ref;
  };

  std::vector<trait_item_candidate> trait_fns;
  mappings->iterate_impl_blocks (
    [&] (HirId id, HIR::ImplBlock *impl) mutable -> bool {
      bool is_trait_impl = impl->has_trait_ref ();
      if (!is_trait_impl)
	return true;

      // look for impl implementation else lookup the associated trait item
      for (auto &impl_item : impl->get_impl_items ())
	{
	  bool is_fn = impl_item->get_impl_item_type ()
		       == HIR::ImplItem::ImplItemType::FUNCTION;
	  if (!is_fn)
	    continue;

	  HIR::Function *func = static_cast<HIR::Function *> (impl_item.get ());
	  if (!func->is_method ())
	    continue;

	  bool name_matches = func->get_function_name ().as_string ().compare (
				segment_name.as_string ())
			      == 0;
	  if (!name_matches)
	    continue;

	  TyTy::BaseType *ty = nullptr;
	  if (!query_type (func->get_mappings ().get_hirid (), &ty))
	    continue;
	  if (ty->get_kind () == TyTy::TypeKind::ERROR)
	    continue;

	  rust_assert (ty->get_kind () == TyTy::TypeKind::FNDEF);
	  TyTy::FnType *fnty = static_cast<TyTy::FnType *> (ty);
	  const TyTy::BaseType *impl_self
	    = TypeCheckItem::ResolveImplBlockSelf (*impl);

	  // see:
	  // https://gcc-rust.zulipchat.com/#narrow/stream/266897-general/topic/Method.20Resolution/near/338646280
	  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/method/probe.rs#L650-L660
	  bool impl_self_is_ptr
	    = impl_self->get_kind () == TyTy::TypeKind::POINTER;
	  bool impl_self_is_ref = impl_self->get_kind () == TyTy::TypeKind::REF;
	  if (receiver_is_raw_ptr && impl_self_is_ptr)
	    {
	      const TyTy::PointerType &sptr
		= *static_cast<const TyTy::PointerType *> (impl_self);
	      const TyTy::PointerType &ptr
		= *static_cast<const TyTy::PointerType *> (raw);

	      // we could do this via lang-item assemblies if we refactor this
	      bool mut_match = sptr.mutability () == ptr.mutability ();
	      if (!mut_match)
		continue;
	    }
	  else if (receiver_is_ref && impl_self_is_ref)
	    {
	      const TyTy::ReferenceType &sptr
		= *static_cast<const TyTy::ReferenceType *> (impl_self);
	      const TyTy::ReferenceType &ptr
		= *static_cast<const TyTy::ReferenceType *> (raw);

	      // we could do this via lang-item assemblies if we refactor this
	      bool mut_match = sptr.mutability () == ptr.mutability ();
	      if (!mut_match)
		continue;
	    }

	  inherent_impl_fns.push_back ({func, impl, fnty});
	  return true;
	}

      TraitReference *trait_ref
	= TraitResolver::Resolve (*impl->get_trait_ref ().get ());
      rust_assert (!trait_ref->is_error ());

      auto item_ref
	= trait_ref->lookup_trait_item (segment_name.as_string (),
					TraitItemReference::TraitItemType::FN);
      if (item_ref->is_error ())
	return true;

      const HIR::Trait *trait = trait_ref->get_hir_trait_ref ();
      HIR::TraitItem *item = item_ref->get_hir_trait_item ();
      if (item->get_item_kind () != HIR::TraitItem::TraitItemKind::FUNC)
	return true;

      HIR::TraitItemFunc *func = static_cast<HIR::TraitItemFunc *> (item);
      if (!func->get_decl ().is_method ())
	return true;

      TyTy::BaseType *ty = item_ref->get_tyty ();
      rust_assert (ty->get_kind () == TyTy::TypeKind::FNDEF);
      TyTy::FnType *fnty = static_cast<TyTy::FnType *> (ty);

      trait_item_candidate candidate{func, trait, fnty, trait_ref, item_ref};
      trait_fns.push_back (candidate);

      return true;
    });

  // lookup specified bounds for an associated item
  struct precdicate_candidate
  {
    TyTy::TypeBoundPredicateItem lookup;
    TyTy::FnType *fntype;
  };

  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/method/probe.rs#L580-L694

  rust_debug ("inherent_impl_fns found {%lu}, trait_fns found {%lu}, "
	      "predicate_items found {%lu}",
	      (unsigned long) inherent_impl_fns.size (),
	      (unsigned long) trait_fns.size (),
	      (unsigned long) predicate_items.size ());

  bool found_possible_candidate = false;
  for (const auto &predicate : predicate_items)
    {
      const TyTy::FnType *fn = predicate.fntype;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      rust_debug ("dot-operator predicate fn_self={%s} can_eq receiver={%s}",
		  fn_self->debug_str ().c_str (),
		  receiver.debug_str ().c_str ());

      auto res
	= TypeCoercionRules::TryCoerce (&receiver, fn_self, UNDEF_LOCATION,
					false /*allow-autoderef*/);
      bool ok = !res.is_error ();
      if (ok)
	{
	  std::vector<Adjustment> adjs = append_adjustments (res.adjustments);
	  const TraitReference *trait_ref
	    = predicate.lookup.get_parent ()->get ();
	  const TraitItemReference *trait_item
	    = predicate.lookup.get_raw_item ();

	  PathProbeCandidate::TraitItemCandidate c{trait_ref, trait_item,
						   nullptr};
	  auto try_result = MethodCandidate{
	    PathProbeCandidate (PathProbeCandidate::CandidateType::TRAIT_FUNC,
				fn->clone (), trait_item->get_locus (), c),
	    adjs};
	  result.insert (std::move (try_result));
	  found_possible_candidate = true;
	}
    }
  if (found_possible_candidate)
    {
      return true;
    }

  for (auto &impl_item : inherent_impl_fns)
    {
      bool is_trait_impl_block = impl_item.impl_block->has_trait_ref ();
      if (is_trait_impl_block)
	continue;

      TyTy::FnType *fn = impl_item.ty;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      rust_debug ("dot-operator impl_item fn_self={%s} can_eq receiver={%s}",
		  fn_self->debug_str ().c_str (),
		  receiver.debug_str ().c_str ());

      auto res
	= TypeCoercionRules::TryCoerce (&receiver, fn_self, UNDEF_LOCATION,
					false /*allow-autoderef*/);
      bool ok = !res.is_error ();
      if (ok)
	{
	  std::vector<Adjustment> adjs = append_adjustments (res.adjustments);
	  PathProbeCandidate::ImplItemCandidate c{impl_item.item,
						  impl_item.impl_block};
	  auto try_result = MethodCandidate{
	    PathProbeCandidate (PathProbeCandidate::CandidateType::IMPL_FUNC,
				fn, impl_item.item->get_locus (), c),
	    adjs};
	  result.insert (std::move (try_result));
	  found_possible_candidate = true;
	}
    }
  if (found_possible_candidate)
    {
      return true;
    }

  for (auto &impl_item : inherent_impl_fns)
    {
      bool is_trait_impl_block = impl_item.impl_block->has_trait_ref ();
      if (!is_trait_impl_block)
	continue;

      TyTy::FnType *fn = impl_item.ty;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      rust_debug (
	"dot-operator trait_impl_item fn_self={%s} can_eq receiver={%s}",
	fn_self->debug_str ().c_str (), receiver.debug_str ().c_str ());

      auto res
	= TypeCoercionRules::TryCoerce (&receiver, fn_self, UNDEF_LOCATION,
					false /*allow-autoderef*/);
      bool ok = !res.is_error ();
      if (ok)
	{
	  std::vector<Adjustment> adjs = append_adjustments (res.adjustments);
	  PathProbeCandidate::ImplItemCandidate c{impl_item.item,
						  impl_item.impl_block};
	  auto try_result = MethodCandidate{
	    PathProbeCandidate (PathProbeCandidate::CandidateType::IMPL_FUNC,
				fn, impl_item.item->get_locus (), c),
	    adjs};
	  result.insert (std::move (try_result));
	  found_possible_candidate = true;
	}
    }
  if (found_possible_candidate)
    {
      return true;
    }

  for (auto trait_item : trait_fns)
    {
      TyTy::FnType *fn = trait_item.ty;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      rust_debug ("dot-operator trait_item fn_self={%s} can_eq receiver={%s}",
		  fn_self->debug_str ().c_str (),
		  receiver.debug_str ().c_str ());

      auto res
	= TypeCoercionRules::TryCoerce (&receiver, fn_self, UNDEF_LOCATION,
					false /*allow-autoderef*/);
      bool ok = !res.is_error ();
      if (ok)
	{
	  std::vector<Adjustment> adjs = append_adjustments (res.adjustments);
	  PathProbeCandidate::TraitItemCandidate c{trait_item.reference,
						   trait_item.item_ref,
						   nullptr};
	  auto try_result = MethodCandidate{
	    PathProbeCandidate (PathProbeCandidate::CandidateType::TRAIT_FUNC,
				fn, trait_item.item->get_locus (), c),
	    adjs};
	  result.insert (std::move (try_result));
	  found_possible_candidate = true;
	}
    }

  return found_possible_candidate;
}

std::vector<MethodResolver::predicate_candidate>
MethodResolver::get_predicate_items (
  const HIR::PathIdentSegment &segment_name, const TyTy::BaseType &receiver,
  const std::vector<TyTy::TypeBoundPredicate> &specified_bounds)
{
  std::vector<predicate_candidate> predicate_items;
  for (auto &bound : specified_bounds)
    {
      TyTy::TypeBoundPredicateItem lookup
	= bound.lookup_associated_item (segment_name.as_string ());
      if (lookup.is_error ())
	continue;

      TyTy::BaseType *ty = lookup.get_tyty_for_receiver (&receiver);
      if (ty->get_kind () == TyTy::TypeKind::FNDEF)
	{
	  TyTy::FnType *fnty = static_cast<TyTy::FnType *> (ty);
	  predicate_candidate candidate{lookup, fnty};
	  predicate_items.push_back (candidate);
	}
    }

  return predicate_items;
}

std::vector<Adjustment>
MethodResolver::append_adjustments (const std::vector<Adjustment> &adjs) const
{
  std::vector<Adjustment> combined;
  combined.reserve (adjustments.size () + adjs.size ());

  for (const auto &a : adjustments)
    combined.push_back (a);
  for (const auto &a : adjs)
    combined.push_back (a);

  return combined;
}

} // namespace Resolver
} // namespace Rust
