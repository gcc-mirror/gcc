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

#include "rust-compile-intrinsic.h"
#include "rust-compile-context.h"
#include "rust-builtins.h"
#include "rust-diagnostics.h"
#include "tree-core.h"
#include "rust-intrinsic-handlers.h"
#include "rust-intrinsic-values.h"

namespace Rust {
namespace Compile {

using IValue = Values::Intrinsics;

static const std::map<std::string, Intrinsic::CompileFn> intrinsics = {
  {IValue::OFFSET, handlers::offset},
  {IValue::ARITH_OFFSET, handlers::arith_offset_handler},
  {IValue::WRITE_BYTES, handlers::write_bytes_handler},
  {IValue::SIZE_OF, handlers::sizeof_handler},
  {IValue::SIZE_OF_VAL, handlers::size_of_val_handler},
  {IValue::MIN_ALIGN_OF, handlers::min_align_of_handler},
  {IValue::MIN_ALIGN_OF_VAL, handlers::min_align_of_val_handler},
  {IValue::TRANSMUTE, handlers::transmute},
  {IValue::ROTATE_LEFT, handlers::rotate_left},
  {IValue::ROTATE_RIGHT, handlers::rotate_right},
  {IValue::WRAPPING_ADD, handlers::wrapping_op (PLUS_EXPR)},
  {IValue::WRAPPING_SUB, handlers::wrapping_op (MINUS_EXPR)},
  {IValue::WRAPPING_MUL, handlers::wrapping_op (MULT_EXPR)},
  {IValue::ADD_WITH_OVERFLOW, handlers::op_with_overflow (PLUS_EXPR)},
  {IValue::SUB_WITH_OVERFLOW, handlers::op_with_overflow (MINUS_EXPR)},
  {IValue::MUL_WITH_OVERFLOW, handlers::op_with_overflow (MULT_EXPR)},
  {IValue::COPY, handlers::copy (true)},
  {IValue::COPY_NONOVERLAPPING, handlers::copy (false)},
  {IValue::PREFETCH_READ_DATA, handlers::prefetch_read_data},
  {IValue::PREFETCH_WRITE_DATA, handlers::prefetch_write_data},
  {IValue::ATOMIC_STORE, handlers::atomic_store (__ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_STORE_SEQCST, handlers::atomic_store (__ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_STORE_REL, handlers::atomic_store (__ATOMIC_RELEASE)},
  {IValue::ATOMIC_STORE_RELEASE, handlers::atomic_store (__ATOMIC_RELEASE)},
  {IValue::ATOMIC_STORE_RELAXED, handlers::atomic_store (__ATOMIC_RELAXED)},
  {IValue::ATOMIC_STORE_UNORDERED, handlers::atomic_store (__ATOMIC_RELAXED)},
  {IValue::ATOMIC_LOAD, handlers::atomic_load (__ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_LOAD_SEQCST, handlers::atomic_load (__ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_LOAD_ACQ, handlers::atomic_load (__ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_LOAD_ACQUIRE, handlers::atomic_load (__ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_LOAD_RELAXED, handlers::atomic_load (__ATOMIC_RELAXED)},
  {IValue::ATOMIC_LOAD_UNORDERED, handlers::atomic_load (__ATOMIC_RELAXED)},
  {IValue::ATOMIC_XADD, handlers::atomic_xadd (__ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_XADD_ACQ, handlers::atomic_xadd (__ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_XADD_REL, handlers::atomic_xadd (__ATOMIC_RELEASE)},
  {IValue::ATOMIC_XADD_ACQREL, handlers::atomic_xadd (__ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_XADD_RELAXED, handlers::atomic_xadd (__ATOMIC_RELAXED)},
  {IValue::ATOMIC_XSUB, handlers::atomic_xsub (__ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_XSUB_ACQ, handlers::atomic_xsub (__ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_XSUB_REL, handlers::atomic_xsub (__ATOMIC_RELEASE)},
  {IValue::ATOMIC_XSUB_ACQREL, handlers::atomic_xsub (__ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_XSUB_RELAXED, handlers::atomic_xsub (__ATOMIC_RELAXED)},
  {IValue::ATOMIC_AND, handlers::atomic_and (__ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_AND_ACQ, handlers::atomic_and (__ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_AND_REL, handlers::atomic_and (__ATOMIC_RELEASE)},
  {IValue::ATOMIC_AND_ACQREL, handlers::atomic_and (__ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_AND_RELAXED, handlers::atomic_and (__ATOMIC_RELAXED)},
  {IValue::ATOMIC_NAND, handlers::atomic_nand (__ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_NAND_ACQ, handlers::atomic_nand (__ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_NAND_REL, handlers::atomic_nand (__ATOMIC_RELEASE)},
  {IValue::ATOMIC_NAND_ACQREL, handlers::atomic_nand (__ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_NAND_RELAXED, handlers::atomic_nand (__ATOMIC_RELAXED)},
  {IValue::ATOMIC_OR, handlers::atomic_or (__ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_OR_ACQ, handlers::atomic_or (__ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_OR_REL, handlers::atomic_or (__ATOMIC_RELEASE)},
  {IValue::ATOMIC_OR_ACQREL, handlers::atomic_or (__ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_OR_RELAXED, handlers::atomic_or (__ATOMIC_RELAXED)},
  {IValue::ATOMIC_XOR, handlers::atomic_xor (__ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_XOR_ACQ, handlers::atomic_xor (__ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_XOR_REL, handlers::atomic_xor (__ATOMIC_RELEASE)},
  {IValue::ATOMIC_XOR_ACQREL, handlers::atomic_xor (__ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_XOR_RELAXED, handlers::atomic_xor (__ATOMIC_RELAXED)},
  {IValue::ATOMIC_CXCHG,
   handlers::atomic_compare_exchange (false, __ATOMIC_SEQ_CST,
				      __ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_CXCHG_ACQ,
   handlers::atomic_compare_exchange (false, __ATOMIC_ACQUIRE,
				      __ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_CXCHG_REL,
   handlers::atomic_compare_exchange (false, __ATOMIC_RELEASE,
				      __ATOMIC_RELAXED)},
  {IValue::ATOMIC_CXCHG_ACQREL,
   handlers::atomic_compare_exchange (false, __ATOMIC_ACQ_REL,
				      __ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_CXCHG_RELAXED,
   handlers::atomic_compare_exchange (false, __ATOMIC_RELAXED,
				      __ATOMIC_RELAXED)},
  {IValue::ATOMIC_CXCHGWEAK,
   handlers::atomic_compare_exchange (true, __ATOMIC_SEQ_CST,
				      __ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_CXCHGWEAK_ACQ,
   handlers::atomic_compare_exchange (true, __ATOMIC_ACQUIRE,
				      __ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_CXCHGWEAK_REL,
   handlers::atomic_compare_exchange (true, __ATOMIC_RELEASE,
				      __ATOMIC_RELAXED)},
  {IValue::ATOMIC_CXCHGWEAK_ACQREL,
   handlers::atomic_compare_exchange (true, __ATOMIC_ACQ_REL,
				      __ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_CXCHGWEAK_RELAXED,
   handlers::atomic_compare_exchange (true, __ATOMIC_RELAXED,
				      __ATOMIC_RELAXED)},
  {IValue::ATOMIC_MAX,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::SignedMax,
			    __ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_MAX_ACQ,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::SignedMax,
			    __ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_MAX_REL,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::SignedMax,
			    __ATOMIC_RELEASE)},
  {IValue::ATOMIC_MAX_ACQREL,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::SignedMax,
			    __ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_MAX_RELAXED,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::SignedMax,
			    __ATOMIC_RELAXED)},
  {IValue::ATOMIC_UMAX,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::UnsignedMax,
			    __ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_UMAX_ACQ,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::UnsignedMax,
			    __ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_UMAX_REL,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::UnsignedMax,
			    __ATOMIC_RELEASE)},
  {IValue::ATOMIC_UMAX_ACQREL,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::UnsignedMax,
			    __ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_UMAX_RELAXED,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::UnsignedMax,
			    __ATOMIC_RELAXED)},
  {IValue::ATOMIC_MIN,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::SignedMin,
			    __ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_MIN_ACQ,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::SignedMin,
			    __ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_MIN_REL,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::SignedMin,
			    __ATOMIC_RELEASE)},
  {IValue::ATOMIC_MIN_ACQREL,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::SignedMin,
			    __ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_MIN_RELAXED,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::SignedMin,
			    __ATOMIC_RELAXED)},
  {IValue::ATOMIC_UMIN,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::UnsignedMin,
			    __ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_UMIN_ACQ,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::UnsignedMin,
			    __ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_UMIN_REL,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::UnsignedMin,
			    __ATOMIC_RELEASE)},
  {IValue::ATOMIC_UMIN_ACQREL,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::UnsignedMin,
			    __ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_UMIN_RELAXED,
   handlers::atomic_minmax (handlers::AtomicMinMaxKind::UnsignedMin,
			    __ATOMIC_RELAXED)},
  {IValue::ATOMIC_FENCE, handlers::atomic_fence (true, __ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_FENCE_ACQ, handlers::atomic_fence (true, __ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_FENCE_REL, handlers::atomic_fence (true, __ATOMIC_RELEASE)},
  {IValue::ATOMIC_FENCE_ACQREL,
   handlers::atomic_fence (true, __ATOMIC_ACQ_REL)},
  {IValue::ATOMIC_SINGLETHREADFENCE,
   handlers::atomic_fence (false, __ATOMIC_SEQ_CST)},
  {IValue::ATOMIC_SINGLETHREADFENCE_ACQ,
   handlers::atomic_fence (false, __ATOMIC_ACQUIRE)},
  {IValue::ATOMIC_SINGLETHREADFENCE_REL,
   handlers::atomic_fence (false, __ATOMIC_RELEASE)},
  {IValue::ATOMIC_SINGLETHREADFENCE_ACQREL,
   handlers::atomic_fence (false, __ATOMIC_ACQ_REL)},
  {IValue::UNCHECKED_ADD, handlers::unchecked_op (PLUS_EXPR)},
  {IValue::UNCHECKED_SUB, handlers::unchecked_op (MINUS_EXPR)},
  {IValue::UNCHECKED_MUL, handlers::unchecked_op (MULT_EXPR)},
  {IValue::UNCHECKED_DIV, handlers::unchecked_op (TRUNC_DIV_EXPR)},
  {IValue::UNCHECKED_REM, handlers::unchecked_op (TRUNC_MOD_EXPR)},
  {IValue::UNCHECKED_SHL, handlers::unchecked_op (LSHIFT_EXPR)},
  {IValue::UNCHECKED_SHR, handlers::unchecked_op (RSHIFT_EXPR)},
  {IValue::UNINIT, handlers::uninit},
  {IValue::MOVE_VAL_INIT, handlers::move_val_init},
  {IValue::LIKELY, handlers::expect (true)},
  {IValue::UNLIKELY, handlers::expect (false)},
  {IValue::ASSUME, handlers::assume},
  {IValue::TRY, handlers::try_handler (false)},
  {IValue::CATCH_UNWIND, handlers::try_handler (true)},
  {IValue::DISCRIMINANT_VALUE, handlers::discriminant_value},
  {IValue::VARIANT_COUNT, handlers::variant_count},
  {IValue::BSWAP, handlers::bswap_handler},
  {IValue::CTLZ, handlers::ctlz_handler},
  {IValue::CTLZ_NONZERO, handlers::ctlz_nonzero_handler},
  {IValue::CTTZ, handlers::cttz_handler},
  {IValue::CTTZ_NONZERO, handlers::cttz_nonzero_handler},
  {IValue::FADD_FAST, handlers::fop_fast (ArithmeticOrLogicalOperator::ADD)},
  {IValue::FSUB_FAST,
   handlers::fop_fast (ArithmeticOrLogicalOperator::SUBTRACT)},
  {IValue::FMUL_FAST,
   handlers::fop_fast (ArithmeticOrLogicalOperator::MULTIPLY)},
  {IValue::FDIV_FAST, handlers::fop_fast (ArithmeticOrLogicalOperator::DIVIDE)},
  {IValue::FREM_FAST,
   handlers::fop_fast (ArithmeticOrLogicalOperator::MODULUS)},
  {IValue::FLOAT_TO_INT_UNCHECKED, handlers::float_to_int_unchecked},
  {IValue::EXACT_DIV, handlers::exact_div_handler},
  {IValue::BREAKPOINT, handlers::breakpoint},
  {IValue::PREF_ALIGN_OF, handlers::sorry},
  {IValue::ASSERT_ZERO_VALID, handlers::type_assert (TyAssert::ZeroValid)},
  {IValue::ASSERT_INHABITED, handlers::type_assert (TyAssert::Inhabited)},
  {IValue::ASSERT_UNINIT_VALID, handlers::type_assert (TyAssert::UninitValid)},
};

Intrinsics::Intrinsics (Context *ctx) : ctx (ctx) {}

/**
 * Returns a FUNC_DECL corresponding to the intrinsic function FNTYPE. If a
 * corresponding builtin exists, returns it. If not, search in the generic
 * intrinsics declared and delegate the return to the corresponding handler.
 *
 * @param fntype The Rust function type that should be implemented by the
 * compiler
 */
tree
Intrinsics::compile (TyTy::FnType *fntype, location_t expr_locus)
{
  rust_assert (fntype->get_abi () == ABI::INTRINSIC);

  BuiltinsContext &builtin_ctx = BuiltinsContext::get ();

  if (auto builtin
      = builtin_ctx.lookup_simple_builtin (fntype->get_identifier ()))
    return *builtin;

  // is it a more complex intrinsic?
  auto it = intrinsics.find (fntype->get_identifier ());
  if (it != intrinsics.end ())
    return Intrinsic (it->second).compile (ctx, fntype, expr_locus);

  location_t locus = ctx->get_mappings ().lookup_location (fntype->get_ref ());
  rust_error_at (locus, ErrorCode::E0093,
		 "unrecognized intrinsic function: %qs",
		 fntype->get_identifier ().c_str ());

  return error_mark_node;
}

} // namespace Compile
} // namespace Rust
