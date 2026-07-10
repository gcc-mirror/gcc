// Copyright (C) 2026 Free Software Foundation, Inc.
//
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

#ifndef RUST_INTRINSIC_HANDLERS_H
#define RUST_INTRINSIC_HANDLERS_H

#include "rust-compile-context.h"

namespace Rust {
namespace Compile {

enum class Prefetch
{
  Read,
  Write
};

namespace handlers {

namespace inner {
tree wrapping_op (Context *ctx, TyTy::FnType *fntype, tree_code op);

tree atomic_store (Context *ctx, TyTy::FnType *fntype, int ordering);
tree atomic_load (Context *ctx, TyTy::FnType *fntype, int ordering);
inline tree copy (Context *ctx, TyTy::FnType *fntype, bool overlaps);
inline tree expect (Context *ctx, TyTy::FnType *fntype, bool likely);
tree try_handler (Context *ctx, TyTy::FnType *fntype, bool is_new_api);

tree op_with_overflow (Context *ctx, TyTy::FnType *fntype, tree_code op);

inline tree unchecked_op (Context *ctx, TyTy::FnType *fntype, tree_code op);

} // namespace inner

using HandlerBuilder
  = std::function<tree (Context *, TyTy::FnType *, location_t)>;

const HandlerBuilder op_with_overflow (tree_code op);

tree rotate_left (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree rotate_right (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);

const HandlerBuilder wrapping_op (tree_code op);
tree offset (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree sizeof_handler (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree size_of_val_handler (Context *ctx, TyTy::FnType *fntype,
			  location_t expr_locus);
tree min_align_of_handler (Context *ctx, TyTy::FnType *fntype,
			   location_t expr_locus);
tree min_align_of_val_handler (Context *ctx, TyTy::FnType *fntype,
			       location_t expr_locus);
tree transmute (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree rotate (Context *ctx, TyTy::FnType *fntype, tree_code op);
tree uninit (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree move_val_init (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree assume (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree discriminant_value (Context *ctx, TyTy::FnType *fntype,
			 location_t expr_locus);
tree variant_count (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree bswap_handler (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree ctlz_handler (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree ctlz_nonzero_handler (Context *ctx, TyTy::FnType *fntype,
			   location_t expr_locus);
tree cttz_handler (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);
tree cttz_nonzero_handler (Context *ctx, TyTy::FnType *fntype,
			   location_t expr_locus);

tree prefetch_data (Context *ctx, TyTy::FnType *fntype, Prefetch kind);

HandlerBuilder atomic_store (int ordering);

HandlerBuilder atomic_load (int ordering);

const HandlerBuilder unchecked_op (tree_code op);

const HandlerBuilder copy (bool overlaps);

const HandlerBuilder expect (bool likely);

const HandlerBuilder try_handler (bool is_new_api);

tree prefetch_read_data (Context *ctx, TyTy::FnType *fntype,
			 location_t expr_locus);
tree prefetch_write_data (Context *ctx, TyTy::FnType *fntype,
			  location_t expr_locus);
tree sorry (Context *ctx, TyTy::FnType *fntype, location_t expr_locus);

tree write_bytes_handler (Context *ctx, TyTy::FnType *fntype,
			  location_t expr_locus);
tree arith_offset_handler (Context *ctx, TyTy::FnType *fntype,
			   location_t expr_locus);
tree assert_zero_valid_handler (Context *ctx, TyTy::FnType *fntype,
				location_t expr_locus);

} // namespace handlers

} // namespace Compile
} // namespace Rust

#endif /* ! RUST_INTRINSIC_HANDLERS_H */
