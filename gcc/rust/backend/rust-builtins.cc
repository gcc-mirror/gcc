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
  tree math_function_type_f32
    = build_function_type_list (float_type_node, float_type_node, NULL_TREE);

  define_builtin ("sinf32", BUILT_IN_SINF, "__builtin_sinf", "sinf",
		  math_function_type_f32, builtin_const);
  define_builtin ("sqrtf32", BUILT_IN_SQRTF, "__builtin_sqrtf", "sqrtf",
		  math_function_type_f32, builtin_const);
}

void
BuiltinsContext::setup ()
{
  setup_math_fns ();
  setup_overflow_fns ();

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
