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
#include "rust-gcc.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check.h"
#include "rust-tyty.h"
#include "tree.h"

namespace Rust {
namespace Compile {

struct IntrinsicCtx
{
  /**
   * Setup the intrinsic's context with the required base info. The various
   * members will then get filled out by the helper calls
   */
  IntrinsicCtx (Context *ctx, TyTy::FnType *fntype, location_t loc)
    : ctx (*ctx), fntype (*fntype), param_vars ({}), param_types ({}),
      fn (error_mark_node), loc (loc)
  {}

  Context &ctx;

  // FIXME: Can we be in a situation where the given fntype is null and this is
  // a null deref?
  TyTy::FnType &fntype;

  std::vector<Bvariable *> param_vars;
  std::vector<tree_node *> param_types;

  std::vector<Bvariable *> block_variables = {};

  tree fn;
  location_t loc;

  void add_statement (tree stmt) { ctx.add_statement (stmt); }

  Analysis::Mappings &get_mappings () { return ctx.get_mappings (); }
  Resolver::TypeCheckContext *get_tyctx () { return ctx.get_tyctx (); }

  /**
   * Some functions require a pointer on the original inner compilation context
   */
  Context *inner () const { return &ctx; }

  /**
   * Implementation details for the common intrinsic compilation logic. These
   * are called by Intrinsic::compile.
   */

  /**
   * Items can be forward compiled which means we may not need to invoke this
   * code. We might also have already compiled this generic function as well.
   */
  tl::optional<tree> check_for_cached_intrinsic ();

  /**
   * Maybe override the Hir Lookups for the substitutions in this context
   */
  void maybe_override_ctx ();

  /**
   * Compile the function proper to TREE
   */
  void compile_intrinsic_function ();

  /**
   * Compile and setup a function's parameters
   */
  void compile_fn_params ();

  /**
   * Set the variables for the intrinsic block to use
   */
  void set_block_variables (std::vector<Bvariable *> &&new_vars)
  {
    block_variables = std::move (new_vars);
  }

  void enter_intrinsic_block ();
  void finalize_intrinsic_block ();
};

class Intrinsic
{
public:
  using CompileFn = std::function<void (IntrinsicCtx &)>;

  Intrinsic (CompileFn closure) : closure (closure) {}

  tree compile (Context *ctx, TyTy::FnType *fntype, location_t loc);

private:
  CompileFn closure;
};

enum class Prefetch
{
  Read,
  Write
};

namespace handlers {

namespace inner {
void wrapping_op (IntrinsicCtx &ctx, tree_code op);

void atomic_store (IntrinsicCtx &ctx, int ordering);
void atomic_load (IntrinsicCtx &ctx, int ordering);
inline void copy (IntrinsicCtx &ctx, bool overlaps);
inline void expect (IntrinsicCtx &ctx, bool likely);
void try_handler (IntrinsicCtx &ctx, bool is_new_api);

void op_with_overflow (IntrinsicCtx &ctx, tree_code op);

inline void unchecked_op (IntrinsicCtx &ctx, tree_code op);

} // namespace inner

void rotate_left (IntrinsicCtx &ctx);
void rotate_right (IntrinsicCtx &ctx);

void offset (IntrinsicCtx &ctx);
void sizeof_handler (IntrinsicCtx &ctx);
void size_of_val_handler (IntrinsicCtx &ctx);
void min_align_of_handler (IntrinsicCtx &ctx);
void min_align_of_val_handler (IntrinsicCtx &ctx);
void transmute (IntrinsicCtx &ctx);

// FIXME: This is supposed to be just a ctx without the op right?
// Or are these inner functions and thus shouldn't live here?
void rotate (IntrinsicCtx &ctx, tree_code op);
void prefetch_data (IntrinsicCtx &ctx, Prefetch kind);

void uninit (IntrinsicCtx &ctx);
void move_val_init (IntrinsicCtx &ctx);
void assume (IntrinsicCtx &ctx);
void discriminant_value (IntrinsicCtx &ctx);
void variant_count (IntrinsicCtx &ctx);
void bswap_handler (IntrinsicCtx &ctx);
void ctlz_handler (IntrinsicCtx &ctx);
void ctlz_nonzero_handler (IntrinsicCtx &ctx);
void cttz_handler (IntrinsicCtx &ctx);
void cttz_nonzero_handler (IntrinsicCtx &ctx);

void prefetch_read_data (IntrinsicCtx &ctx);
void prefetch_write_data (IntrinsicCtx &ctx);

void sorry (IntrinsicCtx &ctx);

void float_to_int_unchecked (IntrinsicCtx &ctx);

void write_bytes_handler (IntrinsicCtx &ctx);
void arith_offset_handler (IntrinsicCtx &ctx);
void assert_zero_valid_handler (IntrinsicCtx &ctx);

Intrinsic::CompileFn op_with_overflow (tree_code op);

Intrinsic::CompileFn wrapping_op (tree_code op);

Intrinsic::CompileFn atomic_store (int ordering);
Intrinsic::CompileFn atomic_load (int ordering);

Intrinsic::CompileFn unchecked_op (tree_code op);

Intrinsic::CompileFn copy (bool overlaps);
Intrinsic::CompileFn expect (bool likely);
Intrinsic::CompileFn try_handler (bool is_new_api);

/**
 * For fadd_fast, fsub_fast, fmul_fast, fdiv_fast and frem_fast
 */
Intrinsic::CompileFn fop_fast (ArithmeticOrLogicalOperator op);

} // namespace handlers

} // namespace Compile
} // namespace Rust

#endif /* ! RUST_INTRINSIC_HANDLERS_H */
