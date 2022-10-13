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

#include "rust-autoderef.h"
#include "rust-hir-path-probe.h"
#include "rust-hir-dot-operator.h"
#include "rust-hir-trait-resolve.h"

namespace Rust {
namespace Resolver {

static bool
resolve_operator_overload_fn (
  Analysis::RustLangItem::ItemType lang_item_type, const TyTy::BaseType *ty,
  TyTy::FnType **resolved_fn, HIR::ImplItem **impl_item,
  Adjustment::AdjustmentType *requires_ref_adjustment);

TyTy::BaseType *
Adjuster::adjust_type (const std::vector<Adjustment> &adjustments)
{
  if (adjustments.size () == 0)
    return base->clone ();

  return adjustments.back ().get_expected ()->clone ();
}

Adjustment
Adjuster::try_deref_type (const TyTy::BaseType *ty,
			  Analysis::RustLangItem::ItemType deref_lang_item)
{
  HIR::ImplItem *impl_item = nullptr;
  TyTy::FnType *fn = nullptr;
  Adjustment::AdjustmentType requires_ref_adjustment
    = Adjustment::AdjustmentType::ERROR;
  bool operator_overloaded
    = resolve_operator_overload_fn (deref_lang_item, ty, &fn, &impl_item,
				    &requires_ref_adjustment);
  if (!operator_overloaded)
    {
      return Adjustment::get_error ();
    }

  auto resolved_base = fn->get_return_type ()->clone ();
  bool is_valid_type = resolved_base->get_kind () == TyTy::TypeKind::REF;
  if (!is_valid_type)
    return Adjustment::get_error ();

  TyTy::ReferenceType *ref_base
    = static_cast<TyTy::ReferenceType *> (resolved_base);

  Adjustment::AdjustmentType adjustment_type
    = Adjustment::AdjustmentType::ERROR;
  switch (deref_lang_item)
    {
    case Analysis::RustLangItem::ItemType::DEREF:
      adjustment_type = Adjustment::AdjustmentType::DEREF;
      break;

    case Analysis::RustLangItem::ItemType::DEREF_MUT:
      adjustment_type = Adjustment::AdjustmentType::DEREF_MUT;
      break;

    default:
      break;
    }

  return Adjustment::get_op_overload_deref_adjustment (adjustment_type, ty,
						       ref_base, fn, impl_item,
						       requires_ref_adjustment);
}

Adjustment
Adjuster::try_raw_deref_type (const TyTy::BaseType *ty)
{
  bool is_valid_type = ty->get_kind () == TyTy::TypeKind::REF;
  if (!is_valid_type)
    return Adjustment::get_error ();

  const TyTy::ReferenceType *ref_base
    = static_cast<const TyTy::ReferenceType *> (ty);
  auto infered = ref_base->get_base ()->clone ();

  return Adjustment (Adjustment::AdjustmentType::INDIRECTION, ty, infered);
}

Adjustment
Adjuster::try_unsize_type (const TyTy::BaseType *ty)
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
  Analysis::RustLangItem::ItemType lang_item_type, const TyTy::BaseType *ty,
  TyTy::FnType **resolved_fn, HIR::ImplItem **impl_item,
  Adjustment::AdjustmentType *requires_ref_adjustment)
{
  auto context = TypeCheckContext::get ();
  auto mappings = Analysis::Mappings::get ();

  // look up lang item for arithmetic type
  std::string associated_item_name
    = Analysis::RustLangItem::ToString (lang_item_type);
  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  if (!lang_item_defined)
    return false;

  auto segment = HIR::PathIdentSegment (associated_item_name);
  auto candidates
    = MethodResolver::Probe (ty, HIR::PathIdentSegment (associated_item_name),
			     true);

  bool have_implementation_for_lang_item = !candidates.empty ();
  if (!have_implementation_for_lang_item)
    return false;

  // multiple candidates?
  if (candidates.size () > 1)
    {
      // error out? probably not for this case
      return false;
    }

  // Get the adjusted self
  auto candidate = *candidates.begin ();
  Adjuster adj (ty);
  TyTy::BaseType *adjusted_self = adj.adjust_type (candidate.adjustments);

  // is this the case we are recursive
  // handle the case where we are within the impl block for this
  // lang_item otherwise we end up with a recursive operator overload
  // such as the i32 operator overload trait
  TypeCheckContextItem &fn_context = context->peek_context ();
  if (fn_context.get_type () == TypeCheckContextItem::ItemType::IMPL_ITEM)
    {
      auto &impl_item = fn_context.get_impl_item ();
      HIR::ImplBlock *parent = impl_item.first;
      HIR::Function *fn = impl_item.second;

      if (parent->has_trait_ref ()
	  && fn->get_function_name ().compare (associated_item_name) == 0)
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

  TyTy::BaseType *lookup_tyty = candidate.candidate.ty;

  // rust only support impl item deref operator overloading ie you must have an
  // impl block for it
  rust_assert (candidate.candidate.type
	       == PathProbeCandidate::CandidateType::IMPL_FUNC);
  *impl_item = candidate.candidate.item.impl.impl_item;

  rust_assert (lookup_tyty->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::BaseType *lookup = lookup_tyty;
  TyTy::FnType *fn = static_cast<TyTy::FnType *> (lookup);
  rust_assert (fn->is_method ());

  if (fn->needs_substitution ())
    {
      if (ty->get_kind () == TyTy::TypeKind::ADT)
	{
	  const TyTy::ADTType *adt = static_cast<const TyTy::ADTType *> (ty);

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

	  lookup = fn->infer_substitions (Location ());
	  rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);
	  fn = static_cast<TyTy::FnType *> (lookup);

	  Location unify_locus = mappings->lookup_location (ty->get_ref ());
	  TypeCheckBase::unify_site (
	    ty->get_ref (), TyTy::TyWithLocation (fn->get_self_type ()),
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
AutoderefCycle::cycle (const TyTy::BaseType *receiver)
{
  const TyTy::BaseType *r = receiver;
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

      Adjustment deref
	= Adjuster::try_deref_type (r, Analysis::RustLangItem::ItemType::DEREF);
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

      Adjustment deref_mut = Adjuster::try_deref_type (
	r, Analysis::RustLangItem::ItemType::DEREF_MUT);
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
AutoderefCycle::try_autoderefed (const TyTy::BaseType *r)
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
