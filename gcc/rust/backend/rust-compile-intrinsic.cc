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

static const std::map<std::string, handlers::HandlerBuilder> generic_intrinsics
  = {{IValue::OFFSET, handlers::offset},
     {IValue::ARITH_OFFSET, handlers::arith_offset_handler},
     {IValue::WRITE_BYTES, handlers::write_bytes_handler},
     {IValue::ASSERT_ZERO_VALID, handlers::assert_zero_valid_handler},
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
     {IValue::ATOMIC_STORE_SEQCST, handlers::atomic_store (__ATOMIC_SEQ_CST)},
     {IValue::ATOMIC_STORE_RELEASE, handlers::atomic_store (__ATOMIC_RELEASE)},
     {IValue::ATOMIC_STORE_RELAXED, handlers::atomic_store (__ATOMIC_RELAXED)},
     {IValue::ATOMIC_STORE_UNORDERED,
      handlers::atomic_store (__ATOMIC_RELAXED)},
     {IValue::ATOMIC_LOAD_SEQCST, handlers::atomic_load (__ATOMIC_SEQ_CST)},
     {IValue::ATOMIC_LOAD_ACQUIRE, handlers::atomic_load (__ATOMIC_ACQUIRE)},
     {IValue::ATOMIC_LOAD_RELAXED, handlers::atomic_load (__ATOMIC_RELAXED)},
     {IValue::ATOMIC_LOAD_UNORDERED, handlers::atomic_load (__ATOMIC_RELAXED)},
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
     {IValue::CTTZ_NONZERO, handlers::cttz_nonzero_handler}};

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

  tree builtin = error_mark_node;
  BuiltinsContext &builtin_ctx = BuiltinsContext::get ();

  if (builtin_ctx.lookup_simple_builtin (fntype->get_identifier (), &builtin))
    return builtin;

  // is it an generic builtin?
  auto it = generic_intrinsics.find (fntype->get_identifier ());
  if (it != generic_intrinsics.end ())
    return it->second (ctx, fntype, expr_locus);

  location_t locus = ctx->get_mappings ().lookup_location (fntype->get_ref ());
  rust_error_at (locus, ErrorCode::E0093,
		 "unrecognized intrinsic function: %qs",
		 fntype->get_identifier ().c_str ());

  return error_mark_node;
}

} // namespace Compile
} // namespace Rust
