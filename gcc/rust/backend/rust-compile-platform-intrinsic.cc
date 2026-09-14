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

#include "rust-system.h"
#include "rust-compile-context.h"
#include "rust-compile-type.h"
#include "rust-tyty.h"
#include "rust-abi.h"
#include "rust-tree.h"
#include "tree.h"
#include "fold-const.h"
#include "rust-compile-platform-intrinsic.h"

namespace Rust {
namespace Compile {

const std::map<std::string, PlatformIntrinsic::PlatformIntrinsicMapping>
  PlatformIntrinsic::platform_intrinsics = {
    {"simd_add", {OpKind::BINARY, PLUS_EXPR}},
    {"simd_sub", {OpKind::BINARY, MINUS_EXPR}},
    {"simd_mul", {OpKind::BINARY, MULT_EXPR}},
    {"simd_div",
     {OpKind::BINARY,
      TRUNC_DIV_EXPR}}, // uses RDIV_EXPR if operands are real type
    {"simd_shl", {OpKind::BINARY, LSHIFT_EXPR}},
    {"simd_shr", {OpKind::BINARY, RSHIFT_EXPR}},
    {"simd_and", {OpKind::BINARY, BIT_AND_EXPR}},
    {"simd_or", {OpKind::BINARY, BIT_IOR_EXPR}},
    {"simd_xor", {OpKind::BINARY, BIT_XOR_EXPR}},
    {"simd_eq", {OpKind::COMPARISON, EQ_EXPR}},
    {"simd_ne", {OpKind::COMPARISON, NE_EXPR}},
    {"simd_lt", {OpKind::COMPARISON, LT_EXPR}},
    {"simd_le", {OpKind::COMPARISON, LE_EXPR}},
    {"simd_gt", {OpKind::COMPARISON, GT_EXPR}},
    {"simd_ge", {OpKind::COMPARISON, GE_EXPR}},
};

tree
compile_comparison_op (const TyTy::FnType *fntype,
		       const std::vector<tree> &arguments, location_t locus,
		       const tree &result_type, const tree_code op_code)
{
  tree input_type = TREE_TYPE (arguments[0]);
  tree predicate_type = truth_type_for (input_type);

  tree predicate = fold_build2_loc (locus, op_code, predicate_type,
				    arguments[0], arguments[1]);
  return fold_build3_loc (locus, VEC_COND_EXPR, result_type, predicate,
			  build_minus_one_cst (result_type),
			  build_zero_cst (result_type));
}

tree
compile_binary_op (const TyTy::FnType *fntype,
		   const std::vector<tree> &arguments, location_t locus,
		   const tree &result_type, const tree_code op_code)
{
  tree_code final_op_code = op_code;

  // idk any more elegant way to do this
  if (final_op_code == TRUNC_DIV_EXPR
      && TREE_CODE (TREE_TYPE (result_type)) == REAL_TYPE)
    final_op_code = RDIV_EXPR;

  return fold_build2_loc (locus, final_op_code, result_type, arguments[0],
			  arguments[1]);
}

tree
PlatformIntrinsic::compile_call (Context *ctx, TyTy::FnType *fntype,
				 const std::vector<tree> &arguments,
				 location_t locus)
{
  rust_assert (fntype != nullptr);
  rust_assert (fntype->get_abi () == ABI::PLATFORM_INTRINSIC);
  auto *result_ty = fntype->get_return_type ()->destructure ();
  tree result_type = TyTyResolveCompile::compile (ctx, result_ty);

  auto it = platform_intrinsics.find (fntype->get_identifier ());
  if (it == platform_intrinsics.end ())
    {
      // TODO add an error here
      return error_mark_node;
    }

  const auto &mapping = it->second;

  switch (mapping.kind)
    {
    case OpKind::BINARY:
      return compile_binary_op (fntype, arguments, locus, result_type,
				mapping.code);
    case OpKind::COMPARISON:
      return compile_comparison_op (fntype, arguments, locus, result_type,
				    mapping.code);
    default:
      return error_mark_node;
    }
}

} // namespace Compile
} // namespace Rust
