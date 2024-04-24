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

#include "rust-coercion.h"
#include "rust-type-util.h"

namespace Rust {
namespace Resolver {

TypeCoercionRules::CoercionResult
TypeCoercionRules::Coerce (TyTy::BaseType *receiver, TyTy::BaseType *expected,
			   location_t locus, bool allow_autoderef,
			   bool is_cast_site)
{
  TypeCoercionRules resolver (expected, locus, true, allow_autoderef, false,
			      is_cast_site);
  bool ok = resolver.do_coercion (receiver);
  return ok ? resolver.try_result : CoercionResult::get_error ();
}

TypeCoercionRules::CoercionResult
TypeCoercionRules::TryCoerce (TyTy::BaseType *receiver,
			      TyTy::BaseType *expected, location_t locus,
			      bool allow_autoderef, bool is_cast_site)
{
  TypeCoercionRules resolver (expected, locus, false, allow_autoderef, true,
			      is_cast_site);
  bool ok = resolver.do_coercion (receiver);
  return ok ? resolver.try_result : CoercionResult::get_error ();
}

TypeCoercionRules::TypeCoercionRules (TyTy::BaseType *expected,
				      location_t locus, bool emit_errors,
				      bool allow_autoderef, bool try_flag,
				      bool is_cast_site)
  : AutoderefCycle (!allow_autoderef), mappings (Analysis::Mappings::get ()),
    context (TypeCheckContext::get ()), expected (expected), locus (locus),
    try_result (CoercionResult::get_error ()), emit_errors (emit_errors),
    try_flag (try_flag), is_cast_site (is_cast_site)
{}

bool
TypeCoercionRules::do_coercion (TyTy::BaseType *receiver)
{
  // FIXME this is not finished and might be super simplified
  // see:
  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/coercion.rs

  // handle never
  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/coercion.rs#L155
  if (receiver->get_kind () == TyTy::TypeKind::NEVER)
    {
      // Subtle: If we are coercing from `!` to `?T`, where `?T` is an unbound
      // type variable, we want `?T` to fallback to `!` if not
      // otherwise constrained. An example where this arises:
      //
      //     let _: Option<?T> = Some({ return; });
      //
      // here, we would coerce from `!` to `?T`.
      if (expected->has_substitutions_defined () && !expected->is_concrete ())
	{
	  location_t locus = mappings.lookup_location (receiver->get_ref ());
	  TyTy::TyVar implicit_var
	    = TyTy::TyVar::get_implicit_infer_var (locus);
	  try_result = CoercionResult{{}, implicit_var.get_tyty ()};
	  return true;
	}
      else
	{
	  bool expected_is_infer_var
	    = expected->get_kind () == TyTy::TypeKind::INFER;
	  bool expected_is_general_infer_var
	    = expected_is_infer_var
	      && (static_cast<TyTy::InferType *> (expected)->get_infer_kind ()
		  == TyTy::InferType::InferTypeKind::GENERAL);

	  // FIXME this 'expected_is_general_infer_var' case needs to eventually
	  // should go away see: compile/never_type_err1.rs
	  //
	  // I think we need inference obligations to say that yes we have a
	  // general inference variable but we add the oligation to the expected
	  // type that it could default to '!'
	  if (expected_is_general_infer_var)
	    try_result = CoercionResult{{}, receiver};
	  else
	    try_result = CoercionResult{{}, expected->clone ()};

	  return true;
	}
    }

  // unsize
  bool unsafe_error = false;
  CoercionResult unsize_coercion
    = coerce_unsized (receiver, expected, unsafe_error);
  bool valid_unsize_coercion = !unsize_coercion.is_error ();
  if (valid_unsize_coercion)
    {
      try_result = unsize_coercion;
      return true;
    }
  else if (unsafe_error)
    {
      // location_t lhs = mappings.lookup_location (receiver->get_ref ());
      // location_t rhs = mappings.lookup_location (expected->get_ref ());
      // object_unsafe_error (locus, lhs, rhs);
      return false;
    }

  // pointers
  switch (expected->get_kind ())
    {
      case TyTy::TypeKind::POINTER: {
	TyTy::PointerType *ptr = static_cast<TyTy::PointerType *> (expected);
	try_result = coerce_unsafe_ptr (receiver, ptr, ptr->mutability ());
	return !try_result.is_error ();
      }

      case TyTy::TypeKind::REF: {
	TyTy::ReferenceType *ptr
	  = static_cast<TyTy::ReferenceType *> (expected);
	try_result
	  = coerce_borrowed_pointer (receiver, ptr, ptr->mutability ());
	return !try_result.is_error ();
      }
      break;

    default:
      break;
    }

  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/coercion.rs#L210
  switch (receiver->get_kind ())
    {
      default: {
	rust_debug (
	  "do_coercion default unify and infer expected: %s receiver %s",
	  receiver->debug_str ().c_str (), expected->debug_str ().c_str ());
	TyTy::BaseType *result
	  = unify_site_and (receiver->get_ref (),
			    TyTy::TyWithLocation (expected),
			    TyTy::TyWithLocation (receiver),
			    locus /*unify_locus*/, false /*emit_errors*/,
			    !try_flag /*commit_if_ok*/, try_flag /*infer*/,
			    try_flag /*cleanup on error*/);
	if (result->get_kind () != TyTy::TypeKind::ERROR)
	  {
	    try_result = CoercionResult{{}, result};
	    return true;
	  }
      }
      break;
    }

  return !try_result.is_error ();
}

TypeCoercionRules::CoercionResult
TypeCoercionRules::coerce_unsafe_ptr (TyTy::BaseType *receiver,
				      TyTy::PointerType *expected,
				      Mutability to_mutbl)
{
  rust_debug ("coerce_unsafe_ptr(receiver={%s}, expected={%s})",
	      receiver->debug_str ().c_str (), expected->debug_str ().c_str ());

  Mutability from_mutbl = Mutability::Imm;
  TyTy::BaseType *element = nullptr;
  switch (receiver->get_kind ())
    {
      case TyTy::TypeKind::REF: {
	TyTy::ReferenceType *ref
	  = static_cast<TyTy::ReferenceType *> (receiver);
	from_mutbl = ref->mutability ();
	element = ref->get_base ();
      }
      break;

      case TyTy::TypeKind::POINTER: {
	TyTy::PointerType *ref = static_cast<TyTy::PointerType *> (receiver);
	from_mutbl = ref->mutability ();
	element = ref->get_base ();
      }
      break;

      default: {
	// FIXME this can probably turn into a unify_and
	if (receiver->can_eq (expected, false))
	  return CoercionResult{{}, expected->clone ()};

	return CoercionResult::get_error ();
      }
    }

  bool receiver_is_non_ptr = receiver->get_kind () != TyTy::TypeKind::POINTER;
  if (autoderef_flag && receiver_is_non_ptr)
    {
      // it is unsafe to autoderef to raw pointers
      return CoercionResult::get_error ();
    }

  if (!coerceable_mutability (from_mutbl, to_mutbl))
    {
      location_t lhs = mappings.lookup_location (receiver->get_ref ());
      location_t rhs = mappings.lookup_location (expected->get_ref ());
      mismatched_mutability_error (locus, lhs, rhs);
      return TypeCoercionRules::CoercionResult::get_error ();
    }

  TyTy::PointerType *coerced_mutability
    = new TyTy::PointerType (receiver->get_ref (),
			     TyTy::TyVar (element->get_ref ()), to_mutbl);

  rust_debug ("coerce_unsafe_ptr unify-site");

  // this is a really annoying case rust allows casts of any ptr to another ptr
  // types
  //
  //  *?   vs *i32  - simple coercion valid
  //  *?   vs *T    - simple coercion valid
  //  *i32 vs *i32  - simple coercion valid
  //  *i32 vs *u8   - simple coercion not valid but allowed in cast site
  //  *T   vs *u8   - not valid but is allowed in cast site

  TyTy::BaseType *result
    = unify_site_and (receiver->get_ref (), TyTy::TyWithLocation (expected),
		      TyTy::TyWithLocation (coerced_mutability),
		      locus /*unify_locus*/, !try_flag /*emit_errors*/,
		      !try_flag /*commit_if_ok*/,
		      try_flag && !is_cast_site /*infer*/,
		      try_flag /*cleanup on error*/);
  bool unsafe_ptr_coerceion_ok = result->get_kind () != TyTy::TypeKind::ERROR;
  if (unsafe_ptr_coerceion_ok)
    return CoercionResult{{}, result};

  return TypeCoercionRules::CoercionResult::get_error ();
}

/// Reborrows `&mut A` to `&mut B` and `&(mut) A` to `&B`.
/// To match `A` with `B`, autoderef will be performed,
/// calling `deref`/`deref_mut` where necessary.
TypeCoercionRules::CoercionResult
TypeCoercionRules::coerce_borrowed_pointer (TyTy::BaseType *receiver,
					    TyTy::ReferenceType *expected,
					    Mutability to_mutbl)
{
  rust_debug ("coerce_borrowed_pointer(a={%s}, b={%s})",
	      receiver->debug_str ().c_str (), expected->debug_str ().c_str ());

  Mutability from_mutbl = Mutability::Imm;
  switch (receiver->get_kind ())
    {
      case TyTy::TypeKind::REF: {
	TyTy::ReferenceType *from
	  = static_cast<TyTy::ReferenceType *> (receiver);
	from_mutbl = from->mutability ();
      }
      break;

      default: {
	// FIXME
	// we might be able to replace this with a can_eq because we default
	// back to a final unity anyway
	rust_debug ("coerce_borrowed_pointer -- unify");
	TyTy::BaseType *result
	  = unify_site_and (receiver->get_ref (),
			    TyTy::TyWithLocation (receiver),
			    TyTy::TyWithLocation (expected), locus,
			    false /*emit_errors*/, !try_flag /*commit_if_ok*/,
			    try_flag /* infer */,
			    try_flag /*cleanup_on_failure*/);
	bool default_coerceion_ok
	  = result->get_kind () != TyTy::TypeKind::ERROR;
	if (default_coerceion_ok)
	  return CoercionResult{{}, result};

	return TypeCoercionRules::CoercionResult::get_error ();
      }
    }

  if (!coerceable_mutability (from_mutbl, to_mutbl))
    {
      location_t lhs = mappings.lookup_location (receiver->get_ref ());
      location_t rhs = mappings.lookup_location (expected->get_ref ());
      mismatched_mutability_error (locus, lhs, rhs);
      return TypeCoercionRules::CoercionResult::get_error ();
    }

  rust_debug ("coerce_borrowed_pointer -- autoderef cycle");
  AutoderefCycle::cycle (receiver);
  rust_debug ("coerce_borrowed_pointer -- result: [%s] with adjustments: [%zu]",
	      try_result.is_error () ? "failed" : "matched",
	      try_result.adjustments.size ());

  return try_result;
}

// &[T; n] or &mut [T; n] -> &[T]
// or &mut [T; n] -> &mut [T]
// or &Concrete -> &Trait, etc.
TypeCoercionRules::CoercionResult
TypeCoercionRules::coerce_unsized (TyTy::BaseType *source,
				   TyTy::BaseType *target, bool &unsafe_error)
{
  rust_debug ("coerce_unsized(source={%s}, target={%s})",
	      source->debug_str ().c_str (), target->debug_str ().c_str ());

  bool source_is_ref = source->get_kind () == TyTy::TypeKind::REF;
  bool target_is_ref = target->get_kind () == TyTy::TypeKind::REF;
  bool target_is_ptr = target->get_kind () == TyTy::TypeKind::POINTER;

  bool needs_reborrow = false;
  TyTy::BaseType *ty_a = source;
  TyTy::BaseType *ty_b = target;
  Mutability expected_mutability = Mutability::Imm;
  if (source_is_ref && target_is_ref)
    {
      TyTy::ReferenceType *source_ref
	= static_cast<TyTy::ReferenceType *> (source);
      TyTy::ReferenceType *target_ref
	= static_cast<TyTy::ReferenceType *> (target);

      Mutability from_mutbl = source_ref->mutability ();
      Mutability to_mutbl = target_ref->mutability ();
      if (!coerceable_mutability (from_mutbl, to_mutbl))
	{
	  unsafe_error = true;
	  location_t lhs = mappings.lookup_location (source->get_ref ());
	  location_t rhs = mappings.lookup_location (target->get_ref ());
	  mismatched_mutability_error (locus, lhs, rhs);
	  return TypeCoercionRules::CoercionResult::get_error ();
	}

      ty_a = source_ref->get_base ();
      ty_b = target_ref->get_base ();
      needs_reborrow = true;
      expected_mutability = to_mutbl;

      adjustments.push_back (
	Adjustment (Adjustment::AdjustmentType::INDIRECTION, source_ref, ty_a));
    }
  else if (source_is_ref && target_is_ptr)
    {
      TyTy::ReferenceType *source_ref
	= static_cast<TyTy::ReferenceType *> (source);
      TyTy::PointerType *target_ref = static_cast<TyTy::PointerType *> (target);

      Mutability from_mutbl = source_ref->mutability ();
      Mutability to_mutbl = target_ref->mutability ();
      if (!coerceable_mutability (from_mutbl, to_mutbl))
	{
	  unsafe_error = true;
	  location_t lhs = mappings.lookup_location (source->get_ref ());
	  location_t rhs = mappings.lookup_location (target->get_ref ());
	  mismatched_mutability_error (locus, lhs, rhs);
	  return TypeCoercionRules::CoercionResult::get_error ();
	}

      ty_a = source_ref->get_base ();
      ty_b = target_ref->get_base ();
      needs_reborrow = true;
      expected_mutability = to_mutbl;

      adjustments.push_back (
	Adjustment (Adjustment::AdjustmentType::INDIRECTION, source_ref, ty_a));
    }

  // FIXME
  // there is a bunch of code to ensure something is coerce able to a dyn trait
  // we need to support but we need to support a few more lang items for that
  // see:
  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/coercion.rs#L582

  const auto a = ty_a;
  const auto b = ty_b;

  bool expect_dyn = b->get_kind () == TyTy::TypeKind::DYNAMIC;
  bool need_unsize = a->get_kind () != TyTy::TypeKind::DYNAMIC;

  if (expect_dyn && need_unsize)
    {
      bool bounds_compatible = b->bounds_compatible (*a, locus, true);
      if (!bounds_compatible)
	{
	  unsafe_error = true;
	  return TypeCoercionRules::CoercionResult::get_error ();
	}

      // return the unsize coercion
      TyTy::BaseType *result = b->clone ();
      // result->set_ref (a->get_ref ());

      // append a dyn coercion adjustment
      adjustments.push_back (Adjustment (Adjustment::UNSIZE, a, result));

      // reborrow if needed
      if (needs_reborrow)
	{
	  TyTy::ReferenceType *reborrow
	    = new TyTy::ReferenceType (source->get_ref (),
				       TyTy::TyVar (result->get_ref ()),
				       expected_mutability);

	  Adjustment::AdjustmentType borrow_type
	    = expected_mutability == Mutability::Imm ? Adjustment::IMM_REF
						     : Adjustment::MUT_REF;
	  adjustments.push_back (Adjustment (borrow_type, result, reborrow));
	  result = reborrow;
	}

      return CoercionResult{adjustments, result};
    }

  adjustments.clear ();
  return TypeCoercionRules::CoercionResult::get_error ();
}

bool
TypeCoercionRules::select (TyTy::BaseType &autoderefed)
{
  rust_debug ("TypeCoercionRules::select autoderefed={%s} can_eq expected={%s}",
	      autoderefed.debug_str ().c_str (),
	      expected->debug_str ().c_str ());

  TyTy::BaseType *result
    = unify_site_and (autoderefed.get_ref (), TyTy::TyWithLocation (expected),
		      TyTy::TyWithLocation (&autoderefed),
		      UNDEF_LOCATION /* locus */, false /*emit_errors*/,
		      false /*commit_if_ok*/, true /*infer*/, true /*cleanup*/);
  bool ok = result->get_kind () != TyTy::TypeKind::ERROR;
  if (!ok)
    return false;

  try_result = CoercionResult{adjustments, result};
  return true;
}

/// Coercing a mutable reference to an immutable works, while
/// coercing `&T` to `&mut T` should be forbidden.
bool
TypeCoercionRules::coerceable_mutability (Mutability from_mutbl,
					  Mutability to_mutbl)
{
  return to_mutbl == Mutability::Imm || (from_mutbl == to_mutbl);
}

void
TypeCoercionRules::mismatched_mutability_error (location_t expr_locus,
						location_t lhs, location_t rhs)
{
  if (!emit_errors)
    return;

  rich_location r (line_table, expr_locus);
  r.add_range (lhs);
  r.add_range (rhs);
  rust_error_at (r, "mismatched mutability");
}

void
TypeCoercionRules::object_unsafe_error (location_t expr_locus, location_t lhs,
					location_t rhs)
{
  if (!emit_errors)
    return;

  rich_location r (line_table, expr_locus);
  r.add_range (lhs);
  r.add_range (rhs);
  rust_error_at (r, "unsafe unsize coercion");
}

} // namespace Resolver
} // namespace Rust
