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

#include "rust-diagnostics.h"
#include "rust-system.h"
#include "rust-builtins.h"

#include "target.h"
#include "stringpool.h"

namespace Rust {
namespace Compile {

BuiltinsContext &
BuiltinsContext::get ()
{
  static BuiltinsContext instance;
  return instance;
}

bool
BuiltinsContext::lookup_simple_builtin (const std::string &name, tree *builtin)
{
  auto *to_search = &name;

  auto it = rust_intrinsic_to_gcc_builtin.find (name);
  if (it != rust_intrinsic_to_gcc_builtin.end ())
    to_search = &it->second;

  return lookup_gcc_builtin (*to_search, builtin);
}

BuiltinsContext::BuiltinsContext () { setup (); }

/**
 * Define a function type according to `builtin-types.def`
 *
 * *Heavily* inspired by the D frontend's `def_fn_type` function
 */
void
BuiltinsContext::define_function_type (Type def_idx, Type ret_idx,
				       bool is_variadic, size_t n, ...)
{
  va_list list;
  va_start (list, n);

  auto args = std::vector<tree> ();

  for (size_t i = 0; i < n; i++)
    {
      // The argument is an enum Type, but it's promoted to int when passed
      // though '...'.
      auto arg_idx = va_arg (list, int);
      auto arg_type = builtin_types[arg_idx];

      args.emplace_back (arg_type);
    }

  auto return_type = builtin_types[ret_idx];
  if (return_type == error_mark_node)
    {
      // Mark the builtin as not available.
      builtin_types[def_idx] = error_mark_node;
      va_end (list);
      return;
    }

  auto fn_type = NULL_TREE;
  if (is_variadic)
    fn_type = build_varargs_function_type_array (return_type, n, args.data ());
  else
    fn_type = build_function_type_array (return_type, n, args.data ());

  builtin_types[def_idx] = fn_type;
  va_end (list);
}

// Taken directly from the D frontend
static void
build_c_type_nodes (void)
{
  string_type_node = build_pointer_type (char_type_node);
  const_string_type_node = build_pointer_type (
    build_qualified_type (char_type_node, TYPE_QUAL_CONST));

  if (strcmp (UINTMAX_TYPE, "unsigned int") == 0)
    {
      intmax_type_node = integer_type_node;
      uintmax_type_node = unsigned_type_node;
    }
  else if (strcmp (UINTMAX_TYPE, "long unsigned int") == 0)
    {
      intmax_type_node = long_integer_type_node;
      uintmax_type_node = long_unsigned_type_node;
    }
  else if (strcmp (UINTMAX_TYPE, "long long unsigned int") == 0)
    {
      intmax_type_node = long_long_integer_type_node;
      uintmax_type_node = long_long_unsigned_type_node;
    }
  else
    gcc_unreachable ();

  signed_size_type_node = signed_type_for (size_type_node);
  wint_type_node = unsigned_type_node;
  pid_type_node = integer_type_node;
}

/**
 * Define all builtin types in the `builtin_types` array
 */
void
BuiltinsContext::define_builtin_types ()
{
  // This is taken directly from the D frontend's handling of builtins
  auto va_list_ref_type_node = build_reference_type (va_list_type_node);
  auto va_list_arg_type_node = va_list_type_node;

  build_c_type_nodes ();

  auto builtin_type_for_size = [] (int size, bool unsignedp) {
    tree type = lang_hooks.types.type_for_size (size, unsignedp);
    return type ? type : error_mark_node;
  };

#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) builtin_types[ENUM] = VALUE;
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN)                                      \
  define_function_type (ENUM, RETURN, 0, 0);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, A1)                                  \
  define_function_type (ENUM, RETURN, 0, 1, A1);
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, A1, A2)                              \
  define_function_type (ENUM, RETURN, 0, 2, A1, A2);
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, A1, A2, A3)                          \
  define_function_type (ENUM, RETURN, 0, 3, A1, A2, A3);
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, A1, A2, A3, A4)                      \
  define_function_type (ENUM, RETURN, 0, 4, A1, A2, A3, A4);
#define DEF_FUNCTION_TYPE_5(ENUM, RETURN, A1, A2, A3, A4, A5)                  \
  define_function_type (ENUM, RETURN, 0, 5, A1, A2, A3, A4, A5);
#define DEF_FUNCTION_TYPE_6(ENUM, RETURN, A1, A2, A3, A4, A5, A6)              \
  define_function_type (ENUM, RETURN, 0, 6, A1, A2, A3, A4, A5, A6);
#define DEF_FUNCTION_TYPE_7(ENUM, RETURN, A1, A2, A3, A4, A5, A6, A7)          \
  define_function_type (ENUM, RETURN, 0, 7, A1, A2, A3, A4, A5, A6, A7);
#define DEF_FUNCTION_TYPE_8(ENUM, RETURN, A1, A2, A3, A4, A5, A6, A7, A8)      \
  define_function_type (ENUM, RETURN, 0, 8, A1, A2, A3, A4, A5, A6, A7, A8);
#define DEF_FUNCTION_TYPE_9(ENUM, RETURN, A1, A2, A3, A4, A5, A6, A7, A8, A9)  \
  define_function_type (ENUM, RETURN, 0, 9, A1, A2, A3, A4, A5, A6, A7, A8, A9);
#define DEF_FUNCTION_TYPE_10(ENUM, RETURN, A1, A2, A3, A4, A5, A6, A7, A8, A9, \
			     A10)                                              \
  define_function_type (ENUM, RETURN, 0, 10, A1, A2, A3, A4, A5, A6, A7, A8,   \
			A9, A10);
#define DEF_FUNCTION_TYPE_11(ENUM, RETURN, A1, A2, A3, A4, A5, A6, A7, A8, A9, \
			     A10, A11)                                         \
  define_function_type (ENUM, RETURN, 0, 11, A1, A2, A3, A4, A5, A6, A7, A8,   \
			A9, A10, A11);
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN)                                  \
  define_function_type (ENUM, RETURN, 1, 0);
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, A1)                              \
  define_function_type (ENUM, RETURN, 1, 1, A1);
#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, A1, A2)                          \
  define_function_type (ENUM, RETURN, 1, 2, A1, A2);
#define DEF_FUNCTION_TYPE_VAR_3(ENUM, RETURN, A1, A2, A3)                      \
  define_function_type (ENUM, RETURN, 1, 3, A1, A2, A3);
#define DEF_FUNCTION_TYPE_VAR_4(ENUM, RETURN, A1, A2, A3, A4)                  \
  define_function_type (ENUM, RETURN, 1, 4, A1, A2, A3, A4);
#define DEF_FUNCTION_TYPE_VAR_5(ENUM, RETURN, A1, A2, A3, A4, A5)              \
  define_function_type (ENUM, RETURN, 1, 5, A1, A2, A3, A4, A5);
#define DEF_FUNCTION_TYPE_VAR_6(ENUM, RETURN, A1, A2, A3, A4, A5, A6)          \
  define_function_type (ENUM, RETURN, 1, 6, A1, A2, A3, A4, A5, A6);
#define DEF_FUNCTION_TYPE_VAR_7(ENUM, RETURN, A1, A2, A3, A4, A5, A6, A7)      \
  define_function_type (ENUM, RETURN, 1, 7, A1, A2, A3, A4, A5, A6, A7);
#define DEF_FUNCTION_TYPE_VAR_11(ENUM, RETURN, A1, A2, A3, A4, A5, A6, A7, A8, \
				 A9, A10, A11)                                 \
  define_function_type (ENUM, RETURN, 1, 11, A1, A2, A3, A4, A5, A6, A7, A8,   \
			A9, A10, A11);
#define DEF_POINTER_TYPE(ENUM, TYPE)                                           \
  builtin_types[ENUM] = build_pointer_type (builtin_types[TYPE]);

#include "builtin-types.def"

#undef DEF_PRIMITIVE_TYPE
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

  builtin_types[Type::BT_LAST] = NULL_TREE;
}

/**
 * Define all builtin attributes in the `builtin_types` array
 */
void
BuiltinsContext::define_builtin_attributes ()

{
  auto *built_in_attributes = builtin_attributes;

#define DEF_ATTR_NULL_TREE(ENUM) built_in_attributes[(int) ENUM] = NULL_TREE;
#define DEF_ATTR_INT(ENUM, VALUE)                                              \
  built_in_attributes[ENUM] = build_int_cst (NULL_TREE, VALUE);
#define DEF_ATTR_STRING(ENUM, VALUE)                                           \
  built_in_attributes[ENUM] = build_string (strlen (VALUE), VALUE);
#define DEF_ATTR_IDENT(ENUM, STRING)                                           \
  built_in_attributes[ENUM] = get_identifier (STRING);
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN)                        \
  built_in_attributes[ENUM]                                                    \
    = tree_cons (built_in_attributes[PURPOSE], built_in_attributes[VALUE],     \
		 built_in_attributes[CHAIN]);
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_STRING
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
}

/**
 * Define all builtin functions during the first initialization of the
 * `BuiltinsContext`.
 */
void
BuiltinsContext::define_builtins ()
{
  auto *built_in_attributes = builtin_attributes;
  auto build_builtin = [this] (built_in_function fn_code, const char *fn_name,
			       built_in_class fn_class, tree fn_type, bool both,
			       bool fallback, tree attributes, bool implicit) {
    if (fn_type == error_mark_node)
      return;

    static auto to_skip = strlen ("__builtin_");

    auto libname = fn_name + to_skip;
    auto decl = add_builtin_function (fn_name, fn_type, fn_code, fn_class,
				      fallback ? libname : NULL, attributes);

    set_builtin_decl (fn_code, decl, implicit);

    builtin_functions.insert ({std::string (fn_name), decl});
  };

#define DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P, FALLBACK_P,      \
		    NONANSI_P, ATTRS, IMPLICIT, COND)                          \
  if (NAME && COND)                                                            \
    build_builtin (ENUM, NAME, CLASS, builtin_types[TYPE], BOTH_P, FALLBACK_P, \
		   built_in_attributes[ATTRS], IMPLICIT);
#include "builtins.def"
#undef DEF_BUILTIN
}

/**
 * Register direct mappings between Rust functions and GCC builtins
 */
void
BuiltinsContext::register_rust_mappings ()
{
  rust_intrinsic_to_gcc_builtin = {
    {"unreachable", "__builtin_unreachable"},
    {"abort", "__builtin_abort"},

    // Math intrinsics
    {"sqrtf32", "__builtin_sqrtf"},
    {"sqrtf64", "__builtin_sqrt"},

    {"sinf32", "__builtin_sinf"},
    {"sinf64", "__builtin_sin"},

    {"cosf32", "__builtin_cosf"},
    {"cosf64", "__builtin_cos"},

    {"powf32", "__builtin_powf"},
    {"powf64", "__builtin_pow"},

    {"powif32", "__builtin_powif"},
    {"powif64", "__builtin_powi"},

    {"expf32", "__builtin_expf"},
    {"expf64", "__builtin_exp"},

    {"exp2f32", "__builtin_exp2f"},
    {"exp2f64", "__builtin_exp2"},

    {"logf32", "__builtin_logf"},
    {"logf64", "__builtin_log"},

    {"log10f32", "__builtin_log10f"},
    {"log10f64", "__builtin_log10"},

    {"log2f32", "__builtin_log2f"},
    {"log2f64", "__builtin_log2"},

    {"fmaf32", "__builtin_fmaf"},
    {"fmaf64", "__builtin_fma"},

    {"fabsf32", "__builtin_fabsf"},
    {"fabsf64", "__builtin_fabs"},

    {"minnumf32", "__builtin_fminf"},
    {"minnumf64", "__builtin_fmin"},

    {"maxnumf32", "__builtin_fmaxf"},
    {"maxnumf64", "__builtin_fmax"},

    {"copysignf32", "__builtin_copysignf"},
    {"copysignf64", "__builtin_copysign"},

    {"floorf32", "__builtin_floorf"},
    {"floorf64", "__builtin_floor"},

    {"ceilf32", "__builtin_ceilf"},
    {"ceilf64", "__builtin_ceil"},

    {"truncf32", "__builtin_truncf"},
    {"truncf64", "__builtin_trunc"},

    {"rintf32", "__builtin_rintf"},
    {"rintf64", "__builtin_rint"},

    {"nearbyintf32", "__builtin_nearbyintf"},
    {"nearbyintf64", "__builtin_nearbyint"},

    {"roundf32", "__builtin_roundf"},
    {"roundf64", "__builtin_round"},
  };
}

void
BuiltinsContext::setup ()
{
  define_builtin_types ();
  define_builtin_attributes ();
  define_builtins ();

  register_rust_mappings ();
}

bool
BuiltinsContext::lookup_gcc_builtin (const std::string &name, tree *builtin)
{
  auto it = builtin_functions.find (name);
  if (it == builtin_functions.end ())
    return false;

  *builtin = it->second;
  return true;
}

} // namespace Compile
} // namespace Rust
