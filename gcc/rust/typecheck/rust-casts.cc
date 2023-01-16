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

#include "rust-casts.h"

namespace Rust {
namespace Resolver {

TypeCastRules::TypeCastRules (Location locus, TyTy::TyWithLocation from,
			      TyTy::TyWithLocation to)
  : locus (locus), from (from), to (to)
{}

TypeCoercionRules::CoercionResult
TypeCastRules::resolve (Location locus, TyTy::TyWithLocation from,
			TyTy::TyWithLocation to)
{
  TypeCastRules cast_rules (locus, from, to);
  return cast_rules.check ();
}

TypeCoercionRules::CoercionResult
TypeCastRules::check ()
{
  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/cast.rs#L565-L582
  auto possible_coercion
    = TypeCoercionRules::TryCoerce (from.get_ty (), to.get_ty (), locus);
  if (!possible_coercion.is_error ())
    return possible_coercion;

  // try the simple cast rules
  auto simple_cast = cast_rules ();
  if (!simple_cast.is_error ())
    return simple_cast;

  // failed to cast
  emit_cast_error ();
  return TypeCoercionRules::CoercionResult::get_error ();
}

TypeCoercionRules::CoercionResult
TypeCastRules::cast_rules ()
{
  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/cast.rs#L596
  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/cast.rs#L654

  rust_debug ("cast_rules from={%s} to={%s}",
	      from.get_ty ()->debug_str ().c_str (),
	      to.get_ty ()->debug_str ().c_str ());

  switch (from.get_ty ()->get_kind ())
    {
      case TyTy::TypeKind::INFER: {
	TyTy::InferType *from_infer
	  = static_cast<TyTy::InferType *> (from.get_ty ());
	switch (from_infer->get_infer_kind ())
	  {
	  case TyTy::InferType::InferTypeKind::GENERAL:
	    return TypeCoercionRules::CoercionResult{{},
						     to.get_ty ()->clone ()};

	  case TyTy::InferType::InferTypeKind::INTEGRAL:
	    switch (to.get_ty ()->get_kind ())
	      {
	      case TyTy::TypeKind::CHAR:
	      case TyTy::TypeKind::BOOL:
	      case TyTy::TypeKind::USIZE:
	      case TyTy::TypeKind::ISIZE:
	      case TyTy::TypeKind::UINT:
	      case TyTy::TypeKind::INT:
	      case TyTy::TypeKind::POINTER:
		return TypeCoercionRules::CoercionResult{
		  {}, to.get_ty ()->clone ()};

		case TyTy::TypeKind::INFER: {
		  TyTy::InferType *to_infer
		    = static_cast<TyTy::InferType *> (to.get_ty ());

		  switch (to_infer->get_infer_kind ())
		    {
		    case TyTy::InferType::InferTypeKind::GENERAL:
		    case TyTy::InferType::InferTypeKind::INTEGRAL:
		      return TypeCoercionRules::CoercionResult{
			{}, to.get_ty ()->clone ()};

		    default:
		      return TypeCoercionRules::CoercionResult::get_error ();
		    }
		}
		break;

	      default:
		return TypeCoercionRules::CoercionResult::get_error ();
	      }
	    break;

	  case TyTy::InferType::InferTypeKind::FLOAT:
	    switch (to.get_ty ()->get_kind ())
	      {
	      case TyTy::TypeKind::USIZE:
	      case TyTy::TypeKind::ISIZE:
	      case TyTy::TypeKind::UINT:
	      case TyTy::TypeKind::INT:
		return TypeCoercionRules::CoercionResult{
		  {}, to.get_ty ()->clone ()};

		case TyTy::TypeKind::INFER: {
		  TyTy::InferType *to_infer
		    = static_cast<TyTy::InferType *> (to.get_ty ());

		  switch (to_infer->get_infer_kind ())
		    {
		    case TyTy::InferType::InferTypeKind::GENERAL:
		    case TyTy::InferType::InferTypeKind::FLOAT:
		      return TypeCoercionRules::CoercionResult{
			{}, to.get_ty ()->clone ()};

		    default:
		      return TypeCoercionRules::CoercionResult::get_error ();
		    }
		}
		break;

	      default:
		return TypeCoercionRules::CoercionResult::get_error ();
	      }
	    break;
	  }
      }
      break;

    case TyTy::TypeKind::BOOL:
      switch (to.get_ty ()->get_kind ())
	{
	case TyTy::TypeKind::INFER:
	case TyTy::TypeKind::USIZE:
	case TyTy::TypeKind::ISIZE:
	case TyTy::TypeKind::UINT:
	case TyTy::TypeKind::INT:
	  return TypeCoercionRules::CoercionResult{{}, to.get_ty ()->clone ()};

	default:
	  return TypeCoercionRules::CoercionResult::get_error ();
	}
      break;

    case TyTy::TypeKind::CHAR:
    case TyTy::TypeKind::USIZE:
    case TyTy::TypeKind::ISIZE:
    case TyTy::TypeKind::UINT:
    case TyTy::TypeKind::INT:
      switch (to.get_ty ()->get_kind ())
	{
	  case TyTy::TypeKind::CHAR: {
	    // only u8 and char
	    bool was_uint = from.get_ty ()->get_kind () == TyTy::TypeKind::UINT;
	    bool was_u8 = was_uint
			  && (static_cast<TyTy::UintType *> (from.get_ty ())
				->get_uint_kind ()
			      == TyTy::UintType::UintKind::U8);
	    if (was_u8)
	      return TypeCoercionRules::CoercionResult{{},
						       to.get_ty ()->clone ()};
	  }
	  break;

	case TyTy::TypeKind::INFER:
	case TyTy::TypeKind::USIZE:
	case TyTy::TypeKind::ISIZE:
	case TyTy::TypeKind::UINT:
	case TyTy::TypeKind::INT:
	  return TypeCoercionRules::CoercionResult{{}, to.get_ty ()->clone ()};

	default:
	  return TypeCoercionRules::CoercionResult::get_error ();
	}
      break;

    case TyTy::TypeKind::FLOAT:
      switch (to.get_ty ()->get_kind ())
	{
	case TyTy::TypeKind::FLOAT:
	  return TypeCoercionRules::CoercionResult{{}, to.get_ty ()->clone ()};

	  case TyTy::TypeKind::INFER: {
	    TyTy::InferType *to_infer
	      = static_cast<TyTy::InferType *> (to.get_ty ());

	    switch (to_infer->get_infer_kind ())
	      {
	      case TyTy::InferType::InferTypeKind::GENERAL:
	      case TyTy::InferType::InferTypeKind::FLOAT:
		return TypeCoercionRules::CoercionResult{
		  {}, to.get_ty ()->clone ()};

	      default:
		return TypeCoercionRules::CoercionResult::get_error ();
	      }
	  }
	  break;

	default:
	  return TypeCoercionRules::CoercionResult::get_error ();
	}
      break;

    case TyTy::TypeKind::REF:
    case TyTy::TypeKind::POINTER:
      switch (to.get_ty ()->get_kind ())
	{
	case TyTy::TypeKind::REF:
	case TyTy::TypeKind::POINTER:
	  return check_ptr_ptr_cast ();

	  // FIXME can you cast a pointer to a integral type?

	default:
	  return TypeCoercionRules::CoercionResult::get_error ();
	}
      break;

    default:
      return TypeCoercionRules::CoercionResult::get_error ();
    }

  return TypeCoercionRules::CoercionResult::get_error ();
}

TypeCoercionRules::CoercionResult
TypeCastRules::check_ptr_ptr_cast ()
{
  rust_debug ("check_ptr_ptr_cast from={%s} to={%s}",
	      from.get_ty ()->debug_str ().c_str (),
	      to.get_ty ()->debug_str ().c_str ());

  bool from_is_ref = from.get_ty ()->get_kind () == TyTy::TypeKind::REF;
  bool to_is_ref = to.get_ty ()->get_kind () == TyTy::TypeKind::REF;
  bool from_is_ptr = from.get_ty ()->get_kind () == TyTy::TypeKind::POINTER;
  bool to_is_ptr = to.get_ty ()->get_kind () == TyTy::TypeKind::POINTER;

  if (from_is_ptr && to_is_ptr)
    {
      // mutability is ignored here as all pointer usage requires unsafe
      return TypeCoercionRules::CoercionResult{{}, to.get_ty ()->clone ()};
    }
  else if (from_is_ref && to_is_ref)
    {
      // mutability must be coercedable
      TyTy::ReferenceType &f
	= static_cast<TyTy::ReferenceType &> (*from.get_ty ());
      TyTy::ReferenceType &t
	= static_cast<TyTy::ReferenceType &> (*to.get_ty ());

      if (TypeCoercionRules::coerceable_mutability (f.mutability (),
						    t.mutability ()))
	{
	  return TypeCoercionRules::CoercionResult{{}, to.get_ty ()->clone ()};
	}
    }

  return TypeCoercionRules::CoercionResult::get_error ();
}

void
TypeCastRules::emit_cast_error () const
{
  // error[E0604]
  RichLocation r (locus);
  r.add_range (from.get_locus ());
  r.add_range (to.get_locus ());
  rust_error_at (r, "invalid cast %<%s%> to %<%s%>",
		 from.get_ty ()->get_name ().c_str (),
		 to.get_ty ()->get_name ().c_str ());
}

} // namespace Resolver
} // namespace Rust
