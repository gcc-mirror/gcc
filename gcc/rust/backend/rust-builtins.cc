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

#include "rust-builtins.h"

namespace Rust {
namespace Compile {

static const int builtin_const = 1 << 0;
static const int builtin_noreturn = 1 << 1;
static const int builtin_novops = 1 << 2;

BuiltinsContext &
BuiltinsContext::get ()
{
  static BuiltinsContext instance;
  return instance;
}

bool
BuiltinsContext::lookup_simple_builtin (const std::string &name, tree *builtin)
{
  auto it = rust_intrinsic_to_gcc_builtin.find (name);
  if (it == rust_intrinsic_to_gcc_builtin.end ())
    return false;

  return lookup_gcc_builtin (it->second, builtin);
}

BuiltinsContext::BuiltinsContext () { setup (); }

void
BuiltinsContext::setup_overflow_fns ()
{
  tree overflow_type
    = build_varargs_function_type_list (boolean_type_node, NULL_TREE);

  define_builtin ("add_overflow", BUILT_IN_ADD_OVERFLOW,
		  "__builtin_add_overflow", "add_overflow", overflow_type, 0);
  define_builtin ("sub_overflow", BUILT_IN_SUB_OVERFLOW,
		  "__builtin_sub_overflow", "sub_overflow", overflow_type, 0);
  define_builtin ("mul_overflow", BUILT_IN_MUL_OVERFLOW,
		  "__builtin_mul_overflow", "mul_overflow", overflow_type, 0);
}

void
BuiltinsContext::setup_math_fns ()
{
  tree fn_type_f32_to_f32
    = build_function_type_list (float_type_node, float_type_node, NULL_TREE);
  tree fn_type_f64_to_f64
    = build_function_type_list (double_type_node, double_type_node, NULL_TREE);
  tree fn_type_f32_f32_to_f32
    = build_function_type_list (float_type_node, float_type_node,
				float_type_node, NULL_TREE);
  tree fn_type_f64_f64_to_f64
    = build_function_type_list (double_type_node, double_type_node,
				double_type_node, NULL_TREE);
  tree fn_type_f32_i32_to_f32
    = build_function_type_list (float_type_node, float_type_node,
				integer_type_node, NULL_TREE);
  tree fn_type_f64_i32_to_f64
    = build_function_type_list (double_type_node, double_type_node,
				integer_type_node, NULL_TREE);

  define_builtin ("sqrtf32", BUILT_IN_SQRTF, "__builtin_sqrtf", "sqrtf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("sqrtf64", BUILT_IN_SQRT, "__builtin_sqrt", "sqrt",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("powif32", BUILT_IN_POWIF, "__builtin_powif", "powif",
		  fn_type_f32_i32_to_f32, builtin_const);
  define_builtin ("powif64", BUILT_IN_POWI, "__builtin_powi", "powi",
		  fn_type_f64_i32_to_f64, builtin_const);

  define_builtin ("sinf32", BUILT_IN_SINF, "__builtin_sinf", "sinf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("sinf64", BUILT_IN_SIN, "__builtin_sin", "sin",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("cosf32", BUILT_IN_COSF, "__builtin_cosf", "cosf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("cosf64", BUILT_IN_COS, "__builtin_cos", "cos",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("powf32", BUILT_IN_POWF, "__builtin_powf", "powf",
		  fn_type_f32_f32_to_f32, builtin_const);
  define_builtin ("powf64", BUILT_IN_POW, "__builtin_pow", "pow",
		  fn_type_f64_f64_to_f64, builtin_const);

  define_builtin ("expf32", BUILT_IN_EXPF, "__builtin_expf", "expf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("expf64", BUILT_IN_EXP, "__builtin_exp", "exp",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("exp2f32", BUILT_IN_EXP2F, "__builtin_exp2f", "exp2f",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("exp2f64", BUILT_IN_EXP2, "__builtin_exp2", "exp2",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("logf32", BUILT_IN_LOGF, "__builtin_logf", "logf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("logf64", BUILT_IN_LOG, "__builtin_log", "log",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("log10f32", BUILT_IN_LOG10F, "__builtin_log10f", "log10f",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("log10f64", BUILT_IN_LOG10, "__builtin_log10", "log10",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("log2f32", BUILT_IN_LOG2F, "__builtin_log2f", "log2f",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("log2f64", BUILT_IN_LOG2, "__builtin_log2", "log2",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("fmaf32", BUILT_IN_FMAF, "__builtin_fmaf", "fmaf",
		  fn_type_f32_f32_to_f32, builtin_const);
  define_builtin ("fmaf64", BUILT_IN_FMA, "__builtin_fma", "fma",
		  fn_type_f64_f64_to_f64, builtin_const);

  define_builtin ("fabsf32", BUILT_IN_FABSF, "__builtin_fabsf", "fabsf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("fabsf64", BUILT_IN_FABS, "__builtin_fabs", "fabs",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("minnumf32", BUILT_IN_FMINF, "__builtin_fminf", "fminf",
		  fn_type_f32_f32_to_f32, builtin_const);
  define_builtin ("minnumf64", BUILT_IN_FMIN, "__builtin_fmin", "fmin",
		  fn_type_f64_f64_to_f64, builtin_const);

  define_builtin ("maxnumf32", BUILT_IN_FMAXF, "__builtin_fmaxf", "fmaxf",
		  fn_type_f32_f32_to_f32, builtin_const);
  define_builtin ("maxnumf64", BUILT_IN_FMAX, "__builtin_fmax", "fmax",
		  fn_type_f64_f64_to_f64, builtin_const);

  define_builtin ("copysignf32", BUILT_IN_COPYSIGNF, "__builtin_copysignf",
		  "copysignf", fn_type_f32_f32_to_f32, builtin_const);
  define_builtin ("copysignf64", BUILT_IN_COPYSIGN, "__builtin_copysign",
		  "copysign", fn_type_f64_f64_to_f64, builtin_const);

  define_builtin ("floorf32", BUILT_IN_FLOORF, "__builtin_floorf", "floorf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("floorf64", BUILT_IN_FLOOR, "__builtin_floor", "floor",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("ceilf32", BUILT_IN_CEILF, "__builtin_ceilf", "ceilf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("ceilf64", BUILT_IN_CEIL, "__builtin_ceil", "ceil",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("truncf32", BUILT_IN_TRUNCF, "__builtin_truncf", "truncf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("truncf64", BUILT_IN_TRUNC, "__builtin_trunc", "trunc",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("rintf32", BUILT_IN_RINTF, "__builtin_rintf", "rintf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("rintf64", BUILT_IN_RINT, "__builtin_rint", "rint",
		  fn_type_f64_to_f64, builtin_const);

  define_builtin ("nearbyintf32", BUILT_IN_NEARBYINTF, "__builtin_nearbyintf",
		  "nearbyintf", fn_type_f32_to_f32, builtin_const);
  define_builtin ("nearbyintf64", BUILT_IN_NEARBYINT, "__builtin_nearbyint",
		  "nearbyint", fn_type_f64_to_f64, builtin_const);

  define_builtin ("roundf32", BUILT_IN_ROUNDF, "__builtin_roundf", "roundf",
		  fn_type_f32_to_f32, builtin_const);
  define_builtin ("roundf64", BUILT_IN_ROUND, "__builtin_round", "round",
		  fn_type_f64_to_f64, builtin_const);
}

void
BuiltinsContext::setup_atomic_fns ()
{
  auto atomic_store_type
    = build_varargs_function_type_list (void_type_node, NULL_TREE);
  auto atomic_load_type = [] (tree ret_type_node) {
    return build_function_type_list (ret_type_node,
				     ptr_type_node, // const_ptr_type_node?
				     integer_type_node, NULL_TREE);
  };

  // FIXME: These should be the definition for the generic version of the
  // atomic_store builtins, but I cannot get them to work properly. Revisit
  // later. define_builtin ("atomic_store", BUILT_IN_ATOMIC_STORE,
  // "__atomic_store", NULL,
  //   atomic_store_type, 0);
  // define_builtin ("atomic_store_n", BUILT_IN_ATOMIC_STORE_N,
  // "__atomic_store_n",
  //   NULL, atomic_store_type, 0);

  define_builtin ("atomic_store_1", BUILT_IN_ATOMIC_STORE_1, "__atomic_store_1",
		  NULL, atomic_store_type, 0);
  define_builtin ("atomic_store_2", BUILT_IN_ATOMIC_STORE_2, "__atomic_store_2",
		  NULL, atomic_store_type, 0);
  define_builtin ("atomic_store_4", BUILT_IN_ATOMIC_STORE_4, "__atomic_store_4",
		  NULL, atomic_store_type, 0);
  define_builtin ("atomic_store_8", BUILT_IN_ATOMIC_STORE_8, "__atomic_store_8",
		  NULL, atomic_store_type, 0);
  define_builtin ("atomic_store_16", BUILT_IN_ATOMIC_STORE_16,
		  "__atomic_store_16", NULL, atomic_store_type, 0);

  define_builtin ("atomic_load_1", BUILT_IN_ATOMIC_LOAD_1, "__atomic_load_1",
		  NULL, atomic_load_type (integer_type_node), 0);
  define_builtin ("atomic_load_2", BUILT_IN_ATOMIC_LOAD_2, "__atomic_load_2",
		  NULL, atomic_load_type (integer_type_node), 0);
  define_builtin ("atomic_load_4", BUILT_IN_ATOMIC_LOAD_4, "__atomic_load_4",
		  NULL, atomic_load_type (integer_type_node), 0);
  define_builtin ("atomic_load_8", BUILT_IN_ATOMIC_LOAD_8, "__atomic_load_8",
		  NULL, atomic_load_type (integer_type_node), 0);
}

void
BuiltinsContext::setup ()
{
  setup_math_fns ();
  setup_overflow_fns ();
  setup_atomic_fns ();

  define_builtin ("unreachable", BUILT_IN_UNREACHABLE, "__builtin_unreachable",
		  NULL, build_function_type (void_type_node, void_list_node),
		  builtin_const | builtin_noreturn);

  define_builtin ("abort", BUILT_IN_ABORT, "__builtin_abort", "abort",
		  build_function_type (void_type_node, void_list_node),
		  builtin_const | builtin_noreturn);

  define_builtin ("breakpoint", BUILT_IN_TRAP, "__builtin_trap", "breakpoint",
		  build_function_type (void_type_node, void_list_node),
		  builtin_const | builtin_noreturn);

  define_builtin ("memcpy", BUILT_IN_MEMCPY, "__builtin_memcpy", "memcpy",
		  build_function_type_list (build_pointer_type (void_type_node),
					    build_pointer_type (void_type_node),
					    build_pointer_type (void_type_node),
					    size_type_node, NULL_TREE),
		  0);

  define_builtin ("prefetch", BUILT_IN_PREFETCH, "__builtin_prefetch",
		  "prefetch",
		  build_varargs_function_type_list (
		    build_pointer_type (const_ptr_type_node), NULL_TREE),
		  builtin_const);
}

static void
handle_flags (tree decl, int flags)
{
  if (flags & builtin_const)
    TREE_READONLY (decl) = 1;
  if (flags & builtin_noreturn)
    TREE_READONLY (decl) = 1;
  if (flags & builtin_novops)
    DECL_IS_NOVOPS (decl) = 1;
}

void
BuiltinsContext::define_builtin (const std::string rust_name,
				 built_in_function bcode, const char *name,
				 const char *libname, tree fntype, int flags)
{
  tree decl = add_builtin_function (name, fntype, bcode, BUILT_IN_NORMAL,
				    libname, NULL_TREE);
  handle_flags (decl, flags);
  set_builtin_decl (bcode, decl, true);

  this->builtin_functions_[name] = decl;
  if (libname != NULL)
    {
      decl = add_builtin_function (libname, fntype, bcode, BUILT_IN_NORMAL,
				   NULL, NULL_TREE);
      handle_flags (decl, flags);

      this->builtin_functions_[libname] = decl;
    }

  rust_intrinsic_to_gcc_builtin[rust_name] = name;
}

bool
BuiltinsContext::lookup_gcc_builtin (const std::string &name, tree *builtin)
{
  auto it = builtin_functions_.find (name);
  if (it == builtin_functions_.end ())
    return false;

  *builtin = it->second;
  return true;
}

} // namespace Compile
} // namespace Rust
