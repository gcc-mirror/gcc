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

#include "rust-coercion.h"

namespace Rust {
namespace Resolver {

AutoderefTypeCoercion::CoercionResult
AutoderefTypeCoercion::Coerce (TyTy::BaseType *receiver,
			       TyTy::BaseType *expected, Location locus)
{
  AutoderefTypeCoercion resolver (expected, locus);
  bool ok = resolver.do_coercion (receiver);
  return ok ? resolver.try_result : CoercionResult::get_error ();
}

AutoderefTypeCoercion::AutoderefTypeCoercion (TyTy::BaseType *expected,
					      Location locus)
  : AutoderefCycle (false), mappings (Analysis::Mappings::get ()),
    context (TypeCheckContext::get ()), expected (expected), locus (locus),
    try_result (CoercionResult::get_error ())
{}

bool
AutoderefTypeCoercion::do_coercion (TyTy::BaseType *receiver)
{
  // FIXME this is not finished and might be super simplified
  // see:
  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/coercion.rs

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
      // Location lhs = mappings->lookup_location (receiver->get_ref ());
      // Location rhs = mappings->lookup_location (expected->get_ref ());
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

  return !try_result.is_error ();
}

AutoderefTypeCoercion::CoercionResult
AutoderefTypeCoercion::coerce_unsafe_ptr (TyTy::BaseType *receiver,
					  TyTy::PointerType *expected,
					  Mutability to_mutbl)
{
  rust_debug ("coerce_unsafe_ptr(a={%s}, b={%s})",
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
	TyTy::BaseType *result = receiver->unify (expected);
	return CoercionResult{{}, result};
      }
    }

  if (!coerceable_mutability (from_mutbl, to_mutbl))
    {
      Location lhs = mappings->lookup_location (receiver->get_ref ());
      Location rhs = mappings->lookup_location (expected->get_ref ());
      mismatched_mutability_error (locus, lhs, rhs);
      return AutoderefTypeCoercion::CoercionResult::get_error ();
    }

  TyTy::PointerType *result
    = new TyTy::PointerType (receiver->get_ref (),
			     TyTy::TyVar (element->get_ref ()), to_mutbl);
  return CoercionResult{{}, result};
}

/// Reborrows `&mut A` to `&mut B` and `&(mut) A` to `&B`.
/// To match `A` with `B`, autoderef will be performed,
/// calling `deref`/`deref_mut` where necessary.
AutoderefTypeCoercion::CoercionResult
AutoderefTypeCoercion::coerce_borrowed_pointer (TyTy::BaseType *receiver,
						TyTy::ReferenceType *expected,
						Mutability to_mutbl)
{
  rust_debug ("coerce_borrowed_pointer(a={%s}, b={%s})",
	      receiver->debug_str ().c_str (), expected->debug_str ().c_str ());

  Mutability from_mutbl = Mutability::Imm;
  switch (receiver->get_kind ())
    {
      case TyTy::TypeKind::REF: {
	TyTy::ReferenceType *ref
	  = static_cast<TyTy::ReferenceType *> (receiver);
	from_mutbl = ref->mutability ();
      }
      break;

      default: {
	TyTy::BaseType *result = receiver->unify (expected);
	return CoercionResult{{}, result};
      }
    }

  if (!coerceable_mutability (from_mutbl, to_mutbl))
    {
      Location lhs = mappings->lookup_location (receiver->get_ref ());
      Location rhs = mappings->lookup_location (expected->get_ref ());
      mismatched_mutability_error (locus, lhs, rhs);
      return AutoderefTypeCoercion::CoercionResult::get_error ();
    }

  AutoderefCycle::cycle (receiver);
  return try_result;
}

// &[T; n] or &mut [T; n] -> &[T]
// or &mut [T; n] -> &mut [T]
// or &Concrete -> &Trait, etc.
AutoderefTypeCoercion::CoercionResult
AutoderefTypeCoercion::coerce_unsized (TyTy::BaseType *source,
				       TyTy::BaseType *target,
				       bool &unsafe_error)
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
	  Location lhs = mappings->lookup_location (source->get_ref ());
	  Location rhs = mappings->lookup_location (target->get_ref ());
	  mismatched_mutability_error (locus, lhs, rhs);
	  return AutoderefTypeCoercion::CoercionResult::get_error ();
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
	  Location lhs = mappings->lookup_location (source->get_ref ());
	  Location rhs = mappings->lookup_location (target->get_ref ());
	  mismatched_mutability_error (locus, lhs, rhs);
	  return AutoderefTypeCoercion::CoercionResult::get_error ();
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
	  return AutoderefTypeCoercion::CoercionResult::get_error ();
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
  return AutoderefTypeCoercion::CoercionResult::get_error ();
}

bool
AutoderefTypeCoercion::select (const TyTy::BaseType &autoderefed)
{
  if (autoderefed.can_eq (expected, false))
    {
      try_result = CoercionResult{adjustments, autoderefed.clone ()};
      return true;
    }
  return false;
}

/// Coercing a mutable reference to an immutable works, while
/// coercing `&T` to `&mut T` should be forbidden.
bool
AutoderefTypeCoercion::coerceable_mutability (Mutability from_mutbl,
					      Mutability to_mutbl)
{
  return to_mutbl == Mutability::Imm || (from_mutbl == to_mutbl);
}

void
AutoderefTypeCoercion::mismatched_mutability_error (Location expr_locus,
						    Location lhs, Location rhs)
{
  RichLocation r (expr_locus);
  r.add_range (lhs);
  r.add_range (rhs);
  rust_error_at (r, "mismatched mutability");
}

void
AutoderefTypeCoercion::object_unsafe_error (Location expr_locus, Location lhs,
					    Location rhs)
{
  RichLocation r (expr_locus);
  r.add_range (lhs);
  r.add_range (rhs);
  rust_error_at (r, "unsafe unsize coercion");
}

} // namespace Resolver
} // namespace Rust
