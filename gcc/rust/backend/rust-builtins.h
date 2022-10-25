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
  BuiltinsContext ();

  void setup_overflow_fns ();
  void setup_math_fns ();
  void setup_atomic_fns ();

  void setup ();

  // Define a builtin function.  BCODE is the builtin function code
  // defined by builtins.def.  NAME is the name of the builtin function.
  // LIBNAME is the name of the corresponding library function, and is
  // NULL if there isn't one.  FNTYPE is the type of the function.
  // CONST_P is true if the function has the const attribute.
  // NORETURN_P is true if the function has the noreturn attribute.
  void define_builtin (const std::string rust_name, built_in_function bcode,
		       const char *name, const char *libname, tree fntype,
		       int flags);

  bool lookup_gcc_builtin (const std::string &name, tree *builtin);

  // A mapping of the GCC built-ins exposed to GCC Rust.
  std::map<std::string, tree> builtin_functions_;
  std::map<std::string, std::string> rust_intrinsic_to_gcc_builtin;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_BUILTINS_H
