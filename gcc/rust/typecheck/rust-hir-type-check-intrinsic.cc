// Copyright (C) 2026 Free Software Foundation, Inc.

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

#include "rust-hir-type-check-intrinsic.h"
#include "rust-diagnostics.h"
#include "rust-intrinsic-values.h"
#include "rust-system.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

using IValue = Values::Intrinsics;
using IRT = IntrinsicRuleType;

const std::unordered_map<std::string, IntrinsicRules>
  IntrinsicChecker::intrinsic_rules = {
    // fn unreachable() -> !;
    {IValue::UNREACHABLE, {0, {}, IRT::Never}},

    // pub fn sqrtf32(x: f32) -> f32;
    {IValue::SQRTF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn sqrtf64(x: f64) -> f64;
    {IValue::SQRTF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn sinf32(x: f32) -> f32;
    {IValue::SINF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn sinf64(x: f64) -> f64;
    {IValue::SINF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn cosf32(x: f32) -> f32;
    {IValue::COSF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn cosf64(x: f64) -> f64;
    {IValue::COSF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn expf32(x: f32) -> f32;
    {IValue::EXPF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn expf64(x: f64) -> f64;
    {IValue::EXPF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn exp2f32(x: f32) -> f32;
    {IValue::EXP2F32, {0, {IRT::F32}, IRT::F32}},
    // pub fn exp2f64(x: f64) -> f64;
    {IValue::EXP2F64, {0, {IRT::F64}, IRT::F64}},
    // pub fn logf32(x: f32) -> f32;
    {IValue::LOGF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn logf64(x: f64) -> f64;
    {IValue::LOGF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn log10f32(x: f32) -> f32;
    {IValue::LOG10F32, {0, {IRT::F32}, IRT::F32}},
    // pub fn log10f64(x: f64) -> f64;
    {IValue::LOG10F64, {0, {IRT::F64}, IRT::F64}},
    // pub fn log2f32(x: f32) -> f32;
    {IValue::LOG2F32, {0, {IRT::F32}, IRT::F32}},
    // pub fn log2f64(x: f64) -> f64;
    {IValue::LOG2F64, {0, {IRT::F64}, IRT::F64}},
    // pub fn fabsf32(x: f32) -> f32;
    {IValue::FABSF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn fabsf64(x: f64) -> f64;
    {IValue::FABSF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn floorf32(x: f32) -> f32;
    {IValue::FLOORF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn floorf64(x: f64) -> f64;
    {IValue::FLOORF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn ceilf32(x: f32) -> f32;
    {IValue::CEILF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn ceilf64(x: f64) -> f64;
    {IValue::CEILF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn truncf32(x: f32) -> f32;
    {IValue::TRUNCF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn truncf64(x: f64) -> f64;
    {IValue::TRUNCF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn rintf32(x: f32) -> f32;
    {IValue::RINTF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn rintf64(x: f64) -> f64;
    {IValue::RINTF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn nearbyintf32(x: f32) -> f32;
    {IValue::NEARBYINTF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn nearbyintf64(x: f64) -> f64;
    {IValue::NEARBYINTF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn roundf32(x: f32) -> f32;
    {IValue::ROUNDF32, {0, {IRT::F32}, IRT::F32}},
    // pub fn roundf64(x: f64) -> f64;
    {IValue::ROUNDF64, {0, {IRT::F64}, IRT::F64}},
    // pub fn minnumf32(x: f32, y: f32) -> f32;
    {IValue::MINNUMF32, {0, {IRT::F32, IRT::F32}, IRT::F32}},
    // pub fn minnumf64(x: f64, y: f64) -> f64;
    {IValue::MINNUMF64, {0, {IRT::F64, IRT::F64}, IRT::F64}},
    // pub fn maxnumf32(x: f32, y: f32) -> f32;
    {IValue::MAXNUMF32, {0, {IRT::F32, IRT::F32}, IRT::F32}},
    // pub fn maxnumf64(x: f64, y: f64) -> f64;
    {IValue::MAXNUMF64, {0, {IRT::F64, IRT::F64}, IRT::F64}},

    // pub fn powf32(a: f32, x: f32) -> f32;
    {IValue::POWF32, {0, {IRT::F32, IRT::F32}, IRT::F32}},
    // pub fn powf64(a: f64, x: f64) -> f64;
    {IValue::POWF64, {0, {IRT::F64, IRT::F64}, IRT::F64}},
    // pub fn copysignf32(x: f32, y: f32) -> f32;
    {IValue::COPYSIGNF32, {0, {IRT::F32, IRT::F32}, IRT::F32}},
    // pub fn copysignf64(x: f64, y: f64) -> f64;
    {IValue::COPYSIGNF64, {0, {IRT::F64, IRT::F64}, IRT::F64}},

    // pub fn fmaf32(a: f32, b: f32, c: f32) -> f32;
    {IValue::FMAF32, {0, {IRT::F32, IRT::F32, IRT::F32}, IRT::F32}},
    // pub fn fmaf64(a: f64, b: f64, c: f64) -> f64;
    {IValue::FMAF64, {0, {IRT::F64, IRT::F64, IRT::F64}, IRT::F64}},

    // pub fn powif32(a: f32, x: i32) -> f32;
    {IValue::POWIF32, {0, {IRT::F32, IRT::I32}, IRT::F32}},
    // pub fn powif64(a: f64, x: i32) -> f64;
    {IValue::POWIF64, {0, {IRT::F64, IRT::I32}, IRT::F64}},

    // pub fn abort() -> !;
    {IValue::ABORT, {0, {}, IRT::Never}},
    // pub fn offset<T>(dst: *const T, offset: isize) -> *const T;
    {IValue::OFFSET,
     {1, {IRT::ConstPtrFirstGeneric, IRT::Isize}, IRT::ConstPtrFirstGeneric}},
    // pub fn size_of<T>() -> usize;
    {IValue::SIZE_OF, {1, {}, IRT::Usize}},
    // pub fn size_of_val<T: ?Sized>(_: *const T) -> usize;
    {IValue::SIZE_OF_VAL, {1, {IRT::ConstPtrFirstGeneric}, IRT::Usize}},
    // pub fn transmute<T, U>(e: T) -> U;
    {IValue::TRANSMUTE, {2, {IRT::FirstGeneric}, IRT::SecondGeneric}},
    // pub fn add_with_overflow<T: Copy>(x: T, y: T) -> (T, bool);
    {IValue::ADD_WITH_OVERFLOW,
     {1,
      {IRT::FirstGeneric, IRT::FirstGeneric},
      IRT::TupleFirstGenericAndBool}},
    // pub fn sub_with_overflow<T: Copy>(x: T, y: T) -> (T, bool);
    {IValue::SUB_WITH_OVERFLOW,
     {1,
      {IRT::FirstGeneric, IRT::FirstGeneric},
      IRT::TupleFirstGenericAndBool}},
    // pub fn mul_with_overflow<T: Copy>(x: T, y: T) -> (T, bool);
    {IValue::MUL_WITH_OVERFLOW,
     {1,
      {IRT::FirstGeneric, IRT::FirstGeneric},
      IRT::TupleFirstGenericAndBool}},
    // fn copy<T>(src: *const T, dst: *mut T, count: usize);
    {IValue::COPY,
     {1,
      {IRT::ConstPtrFirstGeneric, IRT::MutPtrFirstGeneric, IRT::Usize},
      IRT::Unit}},
    // fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
    {IValue::COPY_NONOVERLAPPING,
     {1,
      {IRT::ConstPtrFirstGeneric, IRT::MutPtrFirstGeneric, IRT::Usize},
      IRT::Unit}},
    // pub fn prefetch_read_data<T>(data: *const T, locality: i32);
    {IValue::PREFETCH_READ_DATA,
     {1, {IRT::ConstPtrFirstGeneric, IRT::I32}, IRT::Unit}},
    // pub fn prefetch_write_data<T>(data: *const T, locality: i32);
    {IValue::PREFETCH_WRITE_DATA,
     {1, {IRT::ConstPtrFirstGeneric, IRT::I32}, IRT::Unit}},
    // pub unsafe fn atomic_store_seqcst<T: Copy>(_dst: *mut T, _val: T);
    {IValue::ATOMIC_STORE_SEQCST,
     {1, {IRT::MutPtrFirstGeneric, IRT::FirstGeneric}, IRT::Unit}},
    // pub unsafe fn atomic_store_release<T: Copy>(_dst: *mut T, _val: T);
    {IValue::ATOMIC_STORE_RELEASE,
     {1, {IRT::MutPtrFirstGeneric, IRT::FirstGeneric}, IRT::Unit}},
    // pub unsafe fn atomic_store_relaxed<T: Copy>(_dst: *mut T, _val: T);
    {IValue::ATOMIC_STORE_RELAXED,
     {1, {IRT::MutPtrFirstGeneric, IRT::FirstGeneric}, IRT::Unit}},
    // pub unsafe fn atomic_store_unordered<T: Copy>(_dst: *mut T, _val: T);
    {IValue::ATOMIC_STORE_UNORDERED,
     {1, {IRT::MutPtrFirstGeneric, IRT::FirstGeneric}, IRT::Unit}},
    // pub unsafe fn atomic_load_seqcst<T: Copy>(_src: *const T) -> T;
    {IValue::ATOMIC_LOAD_SEQCST,
     {1, {IRT::ConstPtrFirstGeneric}, IRT::FirstGeneric}},
    // pub unsafe fn atomic_load_acquire<T: Copy>(_src: *const T) -> T;
    {IValue::ATOMIC_LOAD_ACQUIRE,
     {1, {IRT::ConstPtrFirstGeneric}, IRT::FirstGeneric}},
    // pub unsafe fn atomic_load_relaxed<T: Copy>(_src: *const T) -> T;
    {IValue::ATOMIC_LOAD_RELAXED,
     {1, {IRT::ConstPtrFirstGeneric}, IRT::FirstGeneric}},
    // pub unsafe fn atomic_load_unordered<T: Copy>(_src: *const T) -> T;
    {IValue::ATOMIC_LOAD_UNORDERED,
     {1, {IRT::ConstPtrFirstGeneric}, IRT::FirstGeneric}},
    // pub fn unchecked_add<T: Copy>(x: T, y: T) -> T;
    {IValue::UNCHECKED_ADD,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn unchecked_sub<T: Copy>(x: T, y: T) -> T;
    {IValue::UNCHECKED_SUB,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn unchecked_mul<T: Copy>(x: T, y: T) -> T;
    {IValue::UNCHECKED_MUL,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn unchecked_div<T: Copy>(x: T, y: T) -> T;
    {IValue::UNCHECKED_DIV,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn unchecked_rem<T: Copy>(x: T, y: T) -> T;
    {IValue::UNCHECKED_REM,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn unchecked_shl<T: Copy>(x: T, y: T) -> T;
    {IValue::UNCHECKED_SHL,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn unchecked_shr<T: Copy>(x: T, y: T) -> T;
    {IValue::UNCHECKED_SHR,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub unsafe fn uninit<T>() -> T
    {IValue::UNINIT, {1, {}, IRT::FirstGeneric}},
    // pub fn move_val_init<T>(dst: *mut T, src: T);
    {IValue::MOVE_VAL_INIT,
     {1, {IRT::MutPtrFirstGeneric, IRT::FirstGeneric}, IRT::Unit}},
    // pub fn rotate_left<T: Copy>(x: T, y: T) -> T;
    {IValue::ROTATE_LEFT,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn rotate_right<T: Copy>(x: T, y: T) -> T;
    {IValue::ROTATE_RIGHT,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn wrapping_add<T: Copy>(a: T, b: T) -> T;
    {IValue::WRAPPING_ADD,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn wrapping_sub<T: Copy>(a: T, b: T) -> T;
    {IValue::WRAPPING_SUB,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn wrapping_mul<T: Copy>(a: T, b: T) -> T;
    {IValue::WRAPPING_MUL,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn saturating_add<T: Copy>(a: T, b: T) -> T;
    {IValue::SATURATING_ADD,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn saturating_sub<T: Copy>(a: T, b: T) -> T;
    {IValue::SATURATING_SUB,
     {1, {IRT::FirstGeneric, IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn likely(b: bool) -> bool;
    {IValue::LIKELY, {0, {IRT::Bool}, IRT::Bool}},
    // pub fn unlikely(b: bool) -> bool;
    {IValue::UNLIKELY, {0, {IRT::Bool}, IRT::Bool}},
    // fn discriminant_value<T>(v: &T) -> <T as DiscriminantKind>::Discriminant;
    {IValue::DISCRIMINANT_VALUE,
     {1, {IRT::RefFirstGeneric}, IRT::AssocTypePlaceholder}},
    // pub fn variant_count<T>() -> usize;
    {IValue::VARIANT_COUNT, {1, {}, IRT::Usize}},
    // NOTE: The return value was temporarily set to () instead of i32.
    // pub unsafe fn catch_unwind(
    //    _try_fn: fn(_: *mut u8),
    //    _data: *mut u8,
    //    _catch_fn: fn(_: *mut u8, _: *mut u8),
    // ) -> i32
    {IValue::CATCH_UNWIND,
     {0,
      {IRT::Fn_MutPtrU8, IRT::MutPtrU8, IRT::Fn_MutPtrU8MutPtrU8},
      IRT::Unit}},
    // pub fn r#try(
    // 		try_fn: fn(*mut u8),
    // 		data: *mut u8,
    // 		catch_fn: fn(*mut u8,
    // 		*mut u8)
    // ) -> i32;
    {IValue::TRY,
     {0,
      {IRT::Fn_MutPtrU8, IRT::MutPtrU8, IRT::Fn_MutPtrU8MutPtrU8},
      IRT::I32}},
    // pub fn assume(b: bool);
    {IValue::ASSUME, {0, {IRT::Bool}, IRT::Unit}},
    // pub fn min_align_of<T>() -> usize;
    {IValue::MIN_ALIGN_OF, {1, {}, IRT::Usize}},
    // pub fn min_align_of_val<T: ?Sized>(_: *const T) -> usize;
    {IValue::MIN_ALIGN_OF_VAL, {1, {IRT::ConstPtrFirstGeneric}, IRT::Usize}},
    // pub fn needs_drop<T>() -> bool;
    {IValue::NEEDS_DROP, {1, {}, IRT::Bool}},
    // pub fn caller_location() -> &'static crate::panic::Location<'static>;
    {IValue::CALLER_LOCATION, {0, {}, IRT::RefAdt}},
    // pub fn ctpop<T: Copy>(x: T) -> T;
    {IValue::CTPOP, {1, {IRT::FirstGeneric}, IRT::FirstGeneric}},

    // pub fn ctlz<T: Copy>(x: T) -> T;
    {IValue::CTLZ, {1, {IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn ctlz_nonzero<T: Copy>(x: T) -> T;
    {IValue::CTLZ_NONZERO, {1, {IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn cttz<T: Copy>(x: T) -> T;
    {IValue::CTTZ, {1, {IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn cttz_nonzero<T: Copy>(x: T) -> T;
    {IValue::CTTZ_NONZERO, {1, {IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn bswap<T: Copy>(x: T) -> T;
    {IValue::BSWAP, {1, {IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn bitreverse<T: Copy>(x: T) -> T;
    {IValue::BITREVERSE, {1, {IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn type_id<T: ?Sized + 'static>() -> u64;
    {IValue::TYPE_ID, {1, {}, IRT::U64}},
    // pub fn ptr_guaranteed_eq<T>(ptr: *const T, other: *const T) -> bool;
    {IValue::PTR_GUARANTEED_EQ,
     {1, {IRT::ConstPtrFirstGeneric, IRT::ConstPtrFirstGeneric}, IRT::Bool}},
    // pub fn ptr_guaranteed_ne<T>(ptr: *const T, other: *const T) -> bool;
    {IValue::PTR_GUARANTEED_NE,
     {1, {IRT::ConstPtrFirstGeneric, IRT::ConstPtrFirstGeneric}, IRT::Bool}},
    // pub fn rustc_peek<T>(_: T) -> T;
    {IValue::RUSTC_PEEK, {1, {IRT::FirstGeneric}, IRT::FirstGeneric}},
    // pub fn type_name<T: ?Sized>() -> &'static str;
    {IValue::TYPE_NAME, {1, {}, IRT::RefStaticStr}},
    // pub fn forget<T: ?Sized>(_: T);
    {IValue::FORGET, {1, {IRT::FirstGeneric}, IRT::Unit}},
    // pub fn black_box<T>(mut dummy: T) -> T
    {IValue::BLACK_BOX, {1, {IRT::FirstGeneric}, IRT::FirstGeneric}},

    // pub fn arith_offset<T>(dst: *const T, offset: isize) -> *const T;
    {IValue::ARITH_OFFSET,
     {1, {IRT::ConstPtrFirstGeneric, IRT::Isize}, IRT::ConstPtrFirstGeneric}},
    // fn write_bytes<T>(dst: *mut T, val: u8, count: usize)
    {IValue::WRITE_BYTES,
     {1, {IRT::MutPtrFirstGeneric, IRT::U8, IRT::Usize}, IRT::Unit}},
    // pub fn assert_zero_valid<T>();
    {IValue::ASSERT_ZERO_VALID, {1, {}, IRT::Unit}},
};

IntrinsicCheckResult
IntrinsicChecker::check (const TyTy::FnType *fntype)
{
  auto it = intrinsic_rules.find (fntype->get_identifier ());
  if (it == intrinsic_rules.end ())
    {
      rust_error_at (fntype->get_locus (), ErrorCode::E0093,
		     "unrecognized intrinsic function: %qs",
		     fntype->get_identifier ().c_str ());
      return IntrinsicCheckResult::Invalid;
    }
  auto rule = &it->second;

  if (fntype->get_substs ().size () != rule->generic_count)
    {
      rust_error_at (fntype->get_locus (), ErrorCode::E0094,
		     "intrinsic has wrong number of type parameters: found "
		     "%lu, expected %lu",
		     (unsigned long) fntype->get_substs ().size (),
		     (unsigned long) rule->generic_count);
      return IntrinsicCheckResult::Invalid;
    }

  IntrinsicCheckResult result = IntrinsicCheckResult::Valid;

  if (fntype->get_num_params () != rule->param_types.size ())
    {
      rust_error_at (fntype->get_locus (), ErrorCode::E0308,
		     "intrinsic has wrong number of function parameters: found "
		     "%lu, expected %lu",
		     (unsigned long) fntype->get_params ().size (),
		     (unsigned long) rule->param_types.size ());
      result = IntrinsicCheckResult::Invalid;
    }
  else
    {
      for (size_t i = 0; i < rule->param_types.size (); i++)
	{
	  const TyTy::BaseType *actual_param
	    = fntype->get_params ().at (i).get_type ();
	  IntrinsicRuleType expected_type = rule->param_types.at (i);

	  if (!check_type (actual_param, expected_type, fntype))
	    {
	      rust_error_at (fntype->get_locus (), ErrorCode::E0308,
			     "intrinsic has wrong type");
	      result = IntrinsicCheckResult::Invalid;
	    }
	}
    }

  if (!check_type (fntype->get_return_type (), rule->return_type, fntype))
    {
      rust_error_at (fntype->get_locus (), ErrorCode::E0308,
		     "intrinsic has wrong type");
      result = IntrinsicCheckResult::Invalid;
    }

  if (result == IntrinsicCheckResult::Invalid)
    {
      rust_inform (fntype->get_locus (), "expected fn pointer %qs",
		   expected_intrinsic_as_string (fntype).c_str ());
      rust_inform (fntype->get_locus (), "   found fn pointer %qs",
		   found_intrinsic_as_string (fntype).c_str ());
    }
  return result;
}

bool
IntrinsicChecker::check_type (const TyTy::BaseType *actual,
			      IntrinsicRuleType expected,
			      const TyTy::FnType *fntype)
{
  switch (expected)
    {
    case IRT::Bool:
      return actual->get_kind () == TyTy::TypeKind::BOOL;
    case IRT::F32:
      return actual->get_kind () == TyTy::TypeKind::FLOAT
	     && static_cast<const TyTy::FloatType *> (actual)->get_float_kind ()
		  == TyTy::FloatType::FloatKind::F32;
    case IRT::F64:
      return actual->get_kind () == TyTy::TypeKind::FLOAT
	     && static_cast<const TyTy::FloatType *> (actual)->get_float_kind ()
		  == TyTy::FloatType::FloatKind::F64;
    case IRT::I32:
      return actual->get_kind () == TyTy::TypeKind::INT
	     && static_cast<const TyTy::IntType *> (actual)->get_int_kind ()
		  == TyTy::IntType::IntKind::I32;
    case IRT::Isize:
      return actual->get_kind () == TyTy::TypeKind::ISIZE;
    case IRT::U8:
      return actual->get_kind () == TyTy::TypeKind::UINT
	     && static_cast<const TyTy::UintType *> (actual)->get_uint_kind ()
		  == TyTy::UintType::UintKind::U8;
    case IRT::U64:
      return actual->get_kind () == TyTy::TypeKind::UINT
	     && static_cast<const TyTy::UintType *> (actual)->get_uint_kind ()
		  == TyTy::UintType::UintKind::U64;
    case IRT::Usize:
      return actual->get_kind () == TyTy::TypeKind::USIZE;
    case IRT::Unit:
      return actual->is_unit ();
    case IRT::Never:
      return actual->get_kind () == TyTy::TypeKind::NEVER;

    case IRT::FirstGeneric:
    case IRT::SecondGeneric:
      {
	if (actual->get_kind () != TyTy::TypeKind::PARAM)
	  return false;

	auto param = static_cast<const TyTy::ParamType *> (actual);
	size_t expected_index = (expected == IRT::FirstGeneric) ? 0 : 1;
	if (fntype->get_substs ().size () <= expected_index)
	  return false;
	auto expected_param
	  = fntype->get_substs ().at (expected_index).get_param_ty ();
	return param->get_ty_ref () == expected_param->get_ty_ref ();
      }
    case IRT::ConstPtrFirstGeneric:
      {
	if (actual->get_kind () != TyTy::TypeKind::POINTER)
	  return false;
	auto ptr = static_cast<const TyTy::PointerType *> (actual);
	if (!ptr->is_const ())
	  return false;
	return check_type (ptr->get_base (), IntrinsicRuleType::FirstGeneric,
			   fntype);
      }
    case IRT::MutPtrFirstGeneric:
      {
	if (actual->get_kind () != TyTy::TypeKind::POINTER)
	  return false;
	auto ptr = static_cast<const TyTy::PointerType *> (actual);
	if (!ptr->is_mutable ())
	  return false;
	return check_type (ptr->get_base (), IntrinsicRuleType::FirstGeneric,
			   fntype);
      }
    case IRT::RefFirstGeneric:
      {
	if (actual->get_kind () != TyTy::TypeKind::REF)
	  return false;
	auto ref = static_cast<const TyTy::ReferenceType *> (actual);
	if (ref->is_mutable ())
	  return false;
	return check_type (ref->get_base (), IntrinsicRuleType::FirstGeneric,
			   fntype);
      }
    case IRT::RefAdt:
      {
	if (actual->get_kind () != TyTy::TypeKind::REF)
	  return false;
	auto ref = static_cast<const TyTy::ReferenceType *> (actual);
	if (ref->is_mutable ())
	  return false;
	return ref->get_base ()->get_kind () == TyTy::TypeKind::ADT;
      }
    case IRT::MutPtrU8:
      {
	if (actual->get_kind () != TyTy::TypeKind::POINTER)
	  return false;
	auto ptr = static_cast<const TyTy::PointerType *> (actual);
	if (!ptr->is_mutable ())
	  return false;
	return check_type (ptr->get_base (), IRT::U8, fntype);
      }
    case IRT::RefStaticStr:
      {
	if (actual->get_kind () != TyTy::TypeKind::REF)
	  return false;
	auto ref = static_cast<const TyTy::ReferenceType *> (actual);
	if (ref->is_mutable ())
	  return false;
	return ref->get_base ()->get_kind () == TyTy::TypeKind::STR;
      }
    case IRT::AssocTypePlaceholder:
      return true;

    case IRT::TupleFirstGenericAndBool:
      {
	if (actual->get_kind () != TyTy::TypeKind::TUPLE)
	  return false;
	auto tuple = static_cast<const TyTy::TupleType *> (actual);
	if (tuple->num_fields () != 2)
	  return false;
	return tuple->get_field (1)->get_kind () == TyTy::TypeKind::BOOL
	       && check_type (tuple->get_field (0),
			      IntrinsicRuleType::FirstGeneric, fntype);
      }

    case IRT::Fn_MutPtrU8:
      {
	if (actual->get_kind () != TyTy::TypeKind::FNPTR)
	  return false;
	auto fn = static_cast<const TyTy::FnPtr *> (actual);
	if (!fn->get_return_type ()->is_unit ())
	  return false;
	if (fn->get_num_params () != 1)
	  return false;
	return check_type (fn->get_param_type_at (0), IRT::MutPtrU8, fntype);
      }
    case IRT::Fn_MutPtrU8MutPtrU8:
      {
	if (actual->get_kind () != TyTy::TypeKind::FNPTR)
	  return false;
	auto fn = static_cast<const TyTy::FnPtr *> (actual);
	if (!fn->get_return_type ()->is_unit ())
	  return false;
	if (fn->get_num_params () != 2)
	  return false;
	return check_type (fn->get_param_type_at (0), IRT::MutPtrU8, fntype)
	       && check_type (fn->get_param_type_at (1), IRT::MutPtrU8, fntype);
      }

    default:
      rust_unreachable ();
    };
  return true;
}

static std::string
to_string (const TyTy::BaseType *type, const TyTy::FnType *fntype)
{
  switch (type->get_kind ())
    {
    case TyTy::TypeKind::BOOL:
      return "bool";
    case TyTy::TypeKind::USIZE:
      return "usize";
    case TyTy::TypeKind::ISIZE:
      return "isize";
    case TyTy::TypeKind::NEVER:
      return "!";
    case TyTy::TypeKind::STR:
      return "str";

    case TyTy::TypeKind::INT:
      {
	auto int_ty = static_cast<const TyTy::IntType *> (type);
	switch (int_ty->get_int_kind ())
	  {
	  case TyTy::IntType::IntKind::I8:
	    return "i8";
	  case TyTy::IntType::IntKind::I16:
	    return "i16";
	  case TyTy::IntType::IntKind::I32:
	    return "i32";
	  case TyTy::IntType::IntKind::I64:
	    return "i64";
	  case TyTy::IntType::IntKind::I128:
	    return "i128";
	  }
	rust_unreachable ();
      }

    case TyTy::TypeKind::UINT:
      {
	auto uint_ty = static_cast<const TyTy::UintType *> (type);
	switch (uint_ty->get_uint_kind ())
	  {
	  case TyTy::UintType::UintKind::U8:
	    return "u8";
	  case TyTy::UintType::UintKind::U16:
	    return "u16";
	  case TyTy::UintType::UintKind::U32:
	    return "u32";
	  case TyTy::UintType::UintKind::U64:
	    return "u64";
	  case TyTy::UintType::UintKind::U128:
	    return "u128";
	  }
	rust_unreachable ();
      }

    case TyTy::TypeKind::FLOAT:
      {
	auto float_ty = static_cast<const TyTy::FloatType *> (type);
	switch (float_ty->get_float_kind ())
	  {
	  case TyTy::FloatType::FloatKind::F32:
	    return "f32";
	  case TyTy::FloatType::FloatKind::F64:
	    return "f64";
	  }
	rust_unreachable ();
      }

    case TyTy::TypeKind::PARAM:
      {
	auto param = static_cast<const TyTy::ParamType *> (type);
	if (fntype != nullptr)
	  {
	    auto substs = fntype->get_substs ();
	    for (size_t i = 0; i < substs.size (); i++)
	      {
		if (substs[i].get_param_ty ()->get_ty_ref ()
		    == param->get_ty_ref ())
		  return "P" + std::to_string (i);
	      }
	  }
	return param->get_symbol ();
      }

    case TyTy::TypeKind::POINTER:
      {
	auto ptr = static_cast<const TyTy::PointerType *> (type);
	std::string prefix = ptr->is_mutable () ? "*mut "
			     : ptr->is_const () ? "*const "
						: "*";
	return prefix + to_string (ptr->get_base (), fntype);
      }

    case TyTy::TypeKind::REF:
      {
	auto ref = static_cast<const TyTy::ReferenceType *> (type);
	std::string prefix = ref->is_mutable () ? "&mut " : "&";
	return prefix + to_string (ref->get_base (), fntype);
      }

    case TyTy::TypeKind::TUPLE:
      {
	auto tuple = static_cast<const TyTy::TupleType *> (type);
	if (tuple->num_fields () == 0)
	  return "()";

	std::string res = "(";
	for (size_t i = 0; i < tuple->num_fields (); i++)
	  {
	    if (i > 0)
	      res += ", ";
	    res += to_string (tuple->get_field (i), fntype);
	  }
	res += ")";
	return res;
      }

    case TyTy::TypeKind::FNPTR:
      {
	auto fnptr = static_cast<const TyTy::FnPtr *> (type);
	std::string res = "fn(";
	for (size_t i = 0; i < fnptr->get_num_params (); i++)
	  {
	    if (i > 0)
	      res += ", ";
	    res += to_string (fnptr->get_param_type_at (i), fntype);
	  }
	res += ")";
	if (!fnptr->get_return_type ()->is_unit ()
	    || fnptr->get_return_type ()->get_kind () == TyTy::TypeKind::NEVER)
	  {
	    res += " -> ";
	    res += to_string (fnptr->get_return_type (), fntype);
	  }
	return res;
      }

    case TyTy::TypeKind::ADT:
      {
	auto adt = static_cast<const TyTy::ADTType *> (type);
	return adt->get_name ();
      }

    case TyTy::TypeKind::PLACEHOLDER:
      {
	auto ph = static_cast<const TyTy::PlaceholderType *> (type);
	return "<" + ph->get_symbol () + ">";
      }

    default:
      return type->as_string ();
    }
}

static std::string
to_string (IntrinsicRuleType ty)
{
  switch (ty)
    {
    case IRT::Bool:
      return "bool";
    case IRT::F32:
      return "f32";
    case IRT::F64:
      return "f64";
    case IRT::I32:
      return "i32";
    case IRT::Isize:
      return "isize";
    case IRT::U8:
      return "u8";
    case IRT::U64:
      return "u64";
    case IRT::Usize:
      return "usize";
    case IRT::Unit:
      return "()";
    case IRT::Never:
      return "!";

    case IRT::FirstGeneric:
      return "P0";
    case IRT::SecondGeneric:
      return "P1";

    case IRT::ConstPtrFirstGeneric:
      return "*const P0";
    case IRT::MutPtrFirstGeneric:
      return "*mut P0";
    case IRT::RefFirstGeneric:
      return "&P0";

    case IRT::RefAdt:
      return "&<ADT>";
    case IRT::MutPtrU8:
      return "*mut u8";
    case IRT::RefStaticStr:
      return "&'static str";
    case IRT::AssocTypePlaceholder:
      return "<AssocType>";

    case IRT::TupleFirstGenericAndBool:
      return "(P0, bool)";

    case IRT::Fn_MutPtrU8:
      return "fn(*mut u8)";
    case IRT::Fn_MutPtrU8MutPtrU8:
      return "fn(*mut u8, *mut u8)";
    };
  rust_unreachable ();
}

std::string
IntrinsicChecker::expected_intrinsic_as_string (const TyTy::FnType *fntype)
{
  auto it = intrinsic_rules.find (fntype->get_identifier ());
  rust_assert (it != intrinsic_rules.end ());
  auto rule = &it->second;
  std::stringstream result;
  result << "extern \"rust-intrinsic\" fn(";
  if (rule->param_types.size () > 0)
    {
      for (size_t i = 0; i < rule->param_types.size (); i++)
	{
	  if (i != 0)
	    result << ", ";
	  result << to_string (rule->param_types.at (i));
	}
    }
  result << ")";
  if (rule->return_type != IntrinsicRuleType::Unit)
    {
      result << " -> " << to_string (rule->return_type);
    }
  return result.str ();
}

std::string
IntrinsicChecker::found_intrinsic_as_string (const TyTy::FnType *fntype)
{
  std::stringstream result;
  result << "extern \"rust-intrinsic\" fn(";
  if (fntype->get_num_params () > 0)
    {
      const auto &params = fntype->get_params ();
      for (size_t i = 0; i < params.size (); i++)
	{
	  if (i != 0)
	    result << ", ";
	  result << to_string (params.at (i).get_type (), fntype);
	}
    }
  result << ")";
  if (!fntype->get_return_type ()->is_unit ()
      || fntype->get_return_type ()->get_kind () == TyTy::TypeKind::NEVER)
    {
      result << " -> " << to_string (fntype->get_return_type (), fntype);
    }
  return result.str ();
}

} // namespace Resolver
} // namespace Rust
