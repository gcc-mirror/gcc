// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef RUST_BUILTINS_H
#define RUST_BUILTINS_H

#include "rust-system.h"
#include "rust-tree.h"
#include "langhooks.h"
#include "tree.h"
#include "selftest.h"

namespace Rust {
namespace Compile {

// https://github.com/rust-lang/rust/blob/master/library/core/src/intrinsics.rs
// https://github.com/rust-lang/rust/blob/master/compiler/rustc_codegen_llvm/src/intrinsic.rs
// https://github.com/Rust-GCC/gccrs/issues/658
//
//   let llvm_name = match name {
//     sym::sqrtf32 => "llvm.sqrt.f32",
//     sym::sqrtf64 => "llvm.sqrt.f64",
//     sym::powif32 => "llvm.powi.f32",
//     sym::powif64 => "llvm.powi.f64",
//     sym::sinf32 => "llvm.sin.f32",
//     sym::sinf64 => "llvm.sin.f64",
//     sym::cosf32 => "llvm.cos.f32",
//     sym::cosf64 => "llvm.cos.f64",
//     sym::powf32 => "llvm.pow.f32",
//     sym::powf64 => "llvm.pow.f64",
//     sym::expf32 => "llvm.exp.f32",
//     sym::expf64 => "llvm.exp.f64",
//     sym::exp2f32 => "llvm.exp2.f32",
//     sym::exp2f64 => "llvm.exp2.f64",
//     sym::logf32 => "llvm.log.f32",
//     sym::logf64 => "llvm.log.f64",
//     sym::log10f32 => "llvm.log10.f32",
//     sym::log10f64 => "llvm.log10.f64",
//     sym::log2f32 => "llvm.log2.f32",
//     sym::log2f64 => "llvm.log2.f64",
//     sym::fmaf32 => "llvm.fma.f32",
//     sym::fmaf64 => "llvm.fma.f64",
//     sym::fabsf32 => "llvm.fabs.f32",
//     sym::fabsf64 => "llvm.fabs.f64",
//     sym::minnumf32 => "llvm.minnum.f32",
//     sym::minnumf64 => "llvm.minnum.f64",
//     sym::maxnumf32 => "llvm.maxnum.f32",
//     sym::maxnumf64 => "llvm.maxnum.f64",
//     sym::copysignf32 => "llvm.copysign.f32",
//     sym::copysignf64 => "llvm.copysign.f64",
//     sym::floorf32 => "llvm.floor.f32",
//     sym::floorf64 => "llvm.floor.f64",
//     sym::ceilf32 => "llvm.ceil.f32",
//     sym::ceilf64 => "llvm.ceil.f64",
//     sym::truncf32 => "llvm.trunc.f32",
//     sym::truncf64 => "llvm.trunc.f64",
//     sym::rintf32 => "llvm.rint.f32",
//     sym::rintf64 => "llvm.rint.f64",
//     sym::nearbyintf32 => "llvm.nearbyint.f32",
//     sym::nearbyintf64 => "llvm.nearbyint.f64",
//     sym::roundf32 => "llvm.round.f32",
//     sym::roundf64 => "llvm.round.f64",
//     _ => return None,
// };
// Some(cx.get_intrinsic(&llvm_name))

class BuiltinsContext
{
public:
  static BuiltinsContext &get ();

  bool lookup_simple_builtin (const std::string &name, tree *builtin);

private:
  enum Type
  {
#define DEF_PRIMITIVE_TYPE(NAME, V) NAME,
#define DEF_FUNCTION_TYPE_0(NAME, R) NAME,
#define DEF_FUNCTION_TYPE_1(NAME, R, A1) NAME,
#define DEF_FUNCTION_TYPE_2(NAME, R, A1, A2) NAME,
#define DEF_FUNCTION_TYPE_3(NAME, R, A1, A2, A3) NAME,
#define DEF_FUNCTION_TYPE_4(NAME, R, A1, A2, A3, A4) NAME,
#define DEF_FUNCTION_TYPE_5(NAME, R, A1, A2, A3, A4, A5) NAME,
#define DEF_FUNCTION_TYPE_6(NAME, R, A1, A2, A3, A4, A5, A6) NAME,
#define DEF_FUNCTION_TYPE_7(NAME, R, A1, A2, A3, A4, A5, A6, A7) NAME,
#define DEF_FUNCTION_TYPE_8(NAME, R, A1, A2, A3, A4, A5, A6, A7, A8) NAME,
#define DEF_FUNCTION_TYPE_9(NAME, R, A1, A2, A3, A4, A5, A6, A7, A8, A9) NAME,
#define DEF_FUNCTION_TYPE_10(NAME, R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) \
  NAME,
#define DEF_FUNCTION_TYPE_11(NAME, R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, \
			     A11)                                              \
  NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, R) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, R, A1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, R, A1, A2) NAME,
#define DEF_FUNCTION_TYPE_VAR_3(NAME, R, A1, A2, A3) NAME,
#define DEF_FUNCTION_TYPE_VAR_4(NAME, R, A1, A2, A3, A4) NAME,
#define DEF_FUNCTION_TYPE_VAR_5(NAME, R, A1, A2, A3, A4, A5) NAME,
#define DEF_FUNCTION_TYPE_VAR_6(NAME, R, A1, A2, A3, A4, A5, A6) NAME,
#define DEF_FUNCTION_TYPE_VAR_7(NAME, R, A1, A2, A3, A4, A5, A6, A7) NAME,
#define DEF_FUNCTION_TYPE_VAR_11(NAME, R, A1, A2, A3, A4, A5, A6, A7, A8, A9,  \
				 A10, A11)                                     \
  NAME,
#define DEF_POINTER_TYPE(NAME, TYPE) NAME,

#include "builtin-types.def"

#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_0
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_7
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_9
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_11
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_FUNCTION_TYPE_VAR_11
#undef DEF_POINTER_TYPE

    BT_LAST,
  };

  enum Attr
  {
#define DEF_ATTR_NULL_TREE(ENUM) ENUM,
#define DEF_ATTR_INT(ENUM, VALUE) ENUM,
#define DEF_ATTR_STRING(ENUM, VALUE) ENUM,
#define DEF_ATTR_IDENT(ENUM, STRING) ENUM,
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN) ENUM,

#include "builtin-attrs.def"

#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_STRING
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST

    ATTR_LAST,
  };

  /**
   * All builtin types, as defined in `builtin-types.def`
   *
   * This array is filled by the `define_builtin_types` method, during the first
   * initialization of the `BuiltinsContext`
   */
  tree builtin_types[Type::BT_LAST + 1];

  /**
   * Similarly, this array contains all builtin attributes, as defined in
   * `builtin-attr.def`
   *
   * This array is filled by the `define_builtin_attributes` method, during the
   * first initialization of the `BuiltinsContext`
   */
  tree builtin_attributes[Attr::ATTR_LAST + 1];

  void define_function_type (Type def, Type ret, bool is_variadic, size_t n,
			     ...);
  void define_builtin_types ();
  void define_builtin_attributes ();
  void define_builtins ();

  void register_rust_mappings ();

  BuiltinsContext ();

  void setup_overflow_fns ();
  void setup_math_fns ();
  void setup_atomic_fns ();

  void setup ();

  bool lookup_gcc_builtin (const std::string &name, tree *builtin);

  // A mapping of the GCC built-ins exposed to GCC Rust.
  std::map<std::string, tree> builtin_functions;
  std::map<std::string, std::string> rust_intrinsic_to_gcc_builtin;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_BUILTINS_H
