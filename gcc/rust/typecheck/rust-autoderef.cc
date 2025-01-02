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

#include "rust-autoderef.h"
#include "rust-hir-path-probe.h"
#include "rust-hir-dot-operator.h"
#include "rust-hir-trait-resolve.h"
#include "rust-type-util.h"
#include "rust-substitution-mapper.h"

namespace Rust {
namespace Resolver {

static bool
resolve_operator_overload_fn (
  LangItem::Kind lang_item_type, TyTy::BaseType *ty, TyTy::FnType **resolved_fn,
  Adjustment::AdjustmentType *requires_ref_adjustment);

TyTy::BaseType *
Adjuster::adjust_type (const std::vector<Adjustment> &adjustments)
{
  if (adjustments.size () == 0)
    return base->clone ();

  return adjustments.back ().get_expected ()->clone ();
}

Adjustment
Adjuster::try_deref_type (TyTy::BaseType *ty, LangItem::Kind deref_lang_item)
{
  TyTy::FnType *fn = nullptr;
  Adjustment::AdjustmentType requires_ref_adjustment
    = Adjustment::AdjustmentType::ERROR;
  bool operator_overloaded
    = resolve_operator_overload_fn (deref_lang_item, ty, &fn,
				    &requires_ref_adjustment);
  if (!operator_overloaded)
    {
      return Adjustment::get_error ();
    }

  auto resolved_base = fn->get_return_type ()->destructure ();
  bool is_valid_type = resolved_base->get_kind () == TyTy::TypeKind::REF;
  if (!is_valid_type)
    return Adjustment::get_error ();

  TyTy::ReferenceType *ref_base
    = static_cast<TyTy::ReferenceType *> (resolved_base);

  Adjustment::AdjustmentType adjustment_type
    = Adjustment::AdjustmentType::ERROR;
  switch (deref_lang_item)
    {
    case LangItem::Kind::DEREF:
      adjustment_type = Adjustment::AdjustmentType::DEREF;
      break;

    case LangItem::Kind::DEREF_MUT:
      adjustment_type = Adjustment::AdjustmentType::DEREF_MUT;
      break;

    default:
      break;
    }

  return Adjustment::get_op_overload_deref_adjustment (adjustment_type, ty,
						       ref_base, fn,
						       requires_ref_adjustment);
}

Adjustment
Adjuster::try_raw_deref_type (TyTy::BaseType *ty)
{
  bool is_valid_type = ty->get_kind () == TyTy::TypeKind::REF;
  if (!is_valid_type)
    return Adjustment::get_error ();

  const TyTy::ReferenceType *ref_base
    = static_cast<const TyTy::ReferenceType *> (ty);
  auto infered = ref_base->get_base ()->destructure ();

  return Adjustment (Adjustment::AdjustmentType::INDIRECTION, ty, infered);
}

Adjustment
Adjuster::try_unsize_type (TyTy::BaseType *ty)
{
  bool is_valid_type = ty->get_kind () == TyTy::TypeKind::ARRAY;
  if (!is_valid_type)
    return Adjustment::get_error ();

  auto mappings = Analysis::Mappings::get ();
  auto context = TypeCheckContext::get ();

  const auto ref_base = static_cast<const TyTy::ArrayType *> (ty);
  auto slice_elem = ref_base->get_element_type ();

  auto slice
    = new TyTy::SliceType (mappings->get_next_hir_id (), ty->get_ident ().locus,
			   TyTy::TyVar (slice_elem->get_ref ()));
  context->insert_implicit_type (slice);

  return Adjustment (Adjustment::AdjustmentType::UNSIZE, ty, slice);
}

static bool
resolve_operator_overload_fn (
  LangItem::Kind lang_item_type, TyTy::BaseType *lhs,
  TyTy::FnType **resolved_fn,
  Adjustment::AdjustmentType *requires_ref_adjustment)
{
  auto context = TypeCheckContext::get ();
  auto mappings = Analysis::Mappings::get ();

  // look up lang item for arithmetic type
  std::string associated_item_name = LangItem::ToString (lang_item_type);
  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  if (!lang_item_defined)
    return false;

  // we might be in a static or const context and unknown is fine
  TypeCheckContextItem current_context = TypeCheckContextItem::get_error ();
  if (context->have_function_context ())
    {
      current_context = context->peek_context ();
    }

  // this flags stops recurisve calls to try and deref when none is available
  // which will cause an infinite loop
  bool autoderef_flag = true;
  auto segment = HIR::PathIdentSegment (associated_item_name);
  auto candidates = MethodResolver::Probe (lhs, segment, autoderef_flag);

  // remove any recursive candidates
  std::set<MethodCandidate> resolved_candidates;
  for (auto &c : candidates)
    {
      const TyTy::BaseType *candidate_type = c.candidate.ty;
      rust_assert (candidate_type->get_kind () == TyTy::TypeKind::FNDEF);

      const TyTy::FnType &fn
	= *static_cast<const TyTy::FnType *> (candidate_type);

      DefId current_fn_defid = current_context.get_defid ();
      bool recursive_candidated = fn.get_id () == current_fn_defid;
      if (!recursive_candidated)
	{
	  resolved_candidates.insert (c);
	}
    }

  auto selected_candidates
    = MethodResolver::Select (resolved_candidates, lhs, {});
  bool have_implementation_for_lang_item = selected_candidates.size () > 0;
  if (!have_implementation_for_lang_item)
    return false;

  if (selected_candidates.size () > 1)
    {
      // no need to error out as we are just trying to see if there is a fit
      return false;
    }

  // Get the adjusted self
  MethodCandidate candidate = *selected_candidates.begin ();
  Adjuster adj (lhs);
  TyTy::BaseType *adjusted_self = adj.adjust_type (candidate.adjustments);

  PathProbeCandidate &resolved_candidate = candidate.candidate;
  TyTy::BaseType *lookup_tyty = candidate.candidate.ty;
  rust_assert (lookup_tyty->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::BaseType *lookup = lookup_tyty;
  TyTy::FnType *fn = static_cast<TyTy::FnType *> (lookup);
  rust_assert (fn->is_method ());

  rust_debug ("is_impl_item_candidate: %s",
	      resolved_candidate.is_impl_candidate () ? "true" : "false");

  // in the case where we resolve to a trait bound we have to be careful we are
  // able to do so there is a case where we are currently resolving the deref
  // operator overload function which is generic and this might resolve to the
  // trait item of deref which is not valid as its just another recursive case
  if (current_context.get_type () == TypeCheckContextItem::ItemType::IMPL_ITEM)
    {
      auto &impl_item = current_context.get_impl_item ();
      HIR::ImplBlock *parent = impl_item.first;
      HIR::Function *fn = impl_item.second;

      if (parent->has_trait_ref ()
	  && fn->get_function_name ().as_string ().compare (
	       associated_item_name)
	       == 0)
	{
	  TraitReference *trait_reference
	    = TraitResolver::Lookup (*parent->get_trait_ref ().get ());
	  if (!trait_reference->is_error ())
	    {
	      TyTy::BaseType *lookup = nullptr;
	      bool ok = context->lookup_type (fn->get_mappings ().get_hirid (),
					      &lookup);
	      rust_assert (ok);
	      rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);

	      TyTy::FnType *fntype = static_cast<TyTy::FnType *> (lookup);
	      rust_assert (fntype->is_method ());

	      bool is_lang_item_impl
		= trait_reference->get_mappings ().get_defid ()
		  == respective_lang_item_id;
	      bool self_is_lang_item_self
		= fntype->get_self_type ()->is_equal (*adjusted_self);
	      bool recursive_operator_overload
		= is_lang_item_impl && self_is_lang_item_self;

	      if (recursive_operator_overload)
		return false;
	    }
	}
    }

  // we found a valid operator overload
  fn->prepare_higher_ranked_bounds ();
  rust_debug ("resolved operator overload to: {%u} {%s}",
	      candidate.candidate.ty->get_ref (),
	      candidate.candidate.ty->debug_str ().c_str ());

  if (fn->needs_substitution ())
    {
      if (lhs->get_kind () == TyTy::TypeKind::ADT)
	{
	  const TyTy::ADTType *adt = static_cast<const TyTy::ADTType *> (lhs);

	  auto s = fn->get_self_type ()->get_root ();
	  rust_assert (s->can_eq (adt, false));
	  rust_assert (s->get_kind () == TyTy::TypeKind::ADT);
	  const TyTy::ADTType *self_adt
	    = static_cast<const TyTy::ADTType *> (s);

	  // we need to grab the Self substitutions as the inherit type
	  // parameters for this
	  if (self_adt->needs_substitution ())
	    {
	      rust_assert (adt->was_substituted ());

	      TyTy::SubstitutionArgumentMappings used_args_in_prev_segment
		= GetUsedSubstArgs::From (adt);

	      TyTy::SubstitutionArgumentMappings inherit_type_args
		= self_adt->solve_mappings_from_receiver_for_self (
		  used_args_in_prev_segment);

	      // there may or may not be inherited type arguments
	      if (!inherit_type_args.is_error ())
		{
		  // need to apply the inherited type arguments to the
		  // function
		  lookup = fn->handle_substitions (inherit_type_args);
		}
	    }
	}
      else
	{
	  rust_assert (candidate.adjustments.size () < 2);

	  // lets infer the params for this we could probably fix this up by
	  // actually just performing a substitution of a single param but this
	  // seems more generic i think.
	  //
	  // this is the case where we had say Foo<&Bar>> and we have derefed to
	  // the &Bar and we are trying to match a method self of Bar which
	  // requires another deref which is matched to the deref trait impl of
	  // &&T so this requires another reference and deref call

	  lookup = fn->infer_substitions (UNDEF_LOCATION);
	  rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);
	  fn = static_cast<TyTy::FnType *> (lookup);

	  location_t unify_locus = mappings->lookup_location (lhs->get_ref ());
	  unify_site (lhs->get_ref (),
		      TyTy::TyWithLocation (fn->get_self_type ()),
		      TyTy::TyWithLocation (adjusted_self), unify_locus);

	  lookup = fn;
	}
    }

  if (candidate.adjustments.size () > 0)
    *requires_ref_adjustment = candidate.adjustments.at (0).get_type ();

  *resolved_fn = static_cast<TyTy::FnType *> (lookup);

  return true;
}

AutoderefCycle::AutoderefCycle (bool autoderef_flag)
  : autoderef_flag (autoderef_flag)
{}

AutoderefCycle::~AutoderefCycle () {}

void
AutoderefCycle::try_hook (const TyTy::BaseType &)
{}

bool
AutoderefCycle::cycle (TyTy::BaseType *receiver)
{
  TyTy::BaseType *r = receiver;
  while (true)
    {
      rust_debug ("autoderef try 1: {%s}", r->debug_str ().c_str ());
      if (try_autoderefed (r))
	return true;

      // 4. deref to to 1, if cannot deref then quit
      if (autoderef_flag)
	return false;

      // try unsize
      Adjustment unsize = Adjuster::try_unsize_type (r);
      if (!unsize.is_error ())
	{
	  adjustments.push_back (unsize);
	  auto unsize_r = unsize.get_expected ();

	  rust_debug ("autoderef try unsize: {%s}",
		      unsize_r->debug_str ().c_str ());
	  if (try_autoderefed (unsize_r))
	    return true;

	  adjustments.pop_back ();
	}

      bool is_ptr = receiver->get_kind () == TyTy::TypeKind::POINTER;
      if (is_ptr)
	{
	  // deref of raw pointers is unsafe
	  return false;
	}

      Adjustment deref = Adjuster::try_deref_type (r, LangItem::Kind::DEREF);
      if (!deref.is_error ())
	{
	  auto deref_r = deref.get_expected ();
	  adjustments.push_back (deref);

	  rust_debug ("autoderef try lang-item DEREF: {%s}",
		      deref_r->debug_str ().c_str ());
	  if (try_autoderefed (deref_r))
	    return true;

	  adjustments.pop_back ();
	}

      Adjustment deref_mut
	= Adjuster::try_deref_type (r, LangItem::Kind::DEREF_MUT);
      if (!deref_mut.is_error ())
	{
	  auto deref_r = deref_mut.get_expected ();
	  adjustments.push_back (deref_mut);

	  rust_debug ("autoderef try lang-item DEREF_MUT: {%s}",
		      deref_r->debug_str ().c_str ());
	  if (try_autoderefed (deref_r))
	    return true;

	  adjustments.pop_back ();
	}

      if (!deref_mut.is_error ())
	{
	  auto deref_r = deref_mut.get_expected ();
	  adjustments.push_back (deref_mut);
	  Adjustment raw_deref = Adjuster::try_raw_deref_type (deref_r);
	  adjustments.push_back (raw_deref);
	  deref_r = raw_deref.get_expected ();

	  if (try_autoderefed (deref_r))
	    return true;

	  adjustments.pop_back ();
	  adjustments.pop_back ();
	}

      if (!deref.is_error ())
	{
	  r = deref.get_expected ();
	  adjustments.push_back (deref);
	}
      Adjustment raw_deref = Adjuster::try_raw_deref_type (r);
      if (raw_deref.is_error ())
	return false;

      r = raw_deref.get_expected ();
      adjustments.push_back (raw_deref);
    }
  return false;
}

bool
AutoderefCycle::try_autoderefed (TyTy::BaseType *r)
{
  try_hook (*r);

  // 1. try raw
  if (select (*r))
    return true;

  // 2. try ref
  TyTy::ReferenceType *r1
    = new TyTy::ReferenceType (r->get_ref (), TyTy::TyVar (r->get_ref ()),
			       Mutability::Imm);
  adjustments.push_back (
    Adjustment (Adjustment::AdjustmentType::IMM_REF, r, r1));
  if (select (*r1))
    return true;

  adjustments.pop_back ();

  // 3. try mut ref
  TyTy::ReferenceType *r2
    = new TyTy::ReferenceType (r->get_ref (), TyTy::TyVar (r->get_ref ()),
			       Mutability::Mut);
  adjustments.push_back (
    Adjustment (Adjustment::AdjustmentType::MUT_REF, r, r2));
  if (select (*r2))
    return true;

  adjustments.pop_back ();

  return false;
}

} // namespace Resolver
} // namespace Rust
