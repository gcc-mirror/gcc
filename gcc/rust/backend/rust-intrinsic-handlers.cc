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

#include "rust-intrinsic-handlers.h"
#include "rust-compile-context.h"
#include "rust-compile-type.h"
#include "rust-compile-fnparam.h"
#include "rust-builtins.h"
#include "rust-diagnostics.h"
#include "rust-location.h"
#include "rust-constexpr.h"
#include "rust-session-manager.h"
#include "rust-tree.h"
#include "tree-core.h"
#include "rust-gcc.h"
#include "fold-const.h"
#include "rust-constexpr.h"

// declaration taken from "stringpool.h"
// the get_identifier macro causes compilation issues
extern tree get_identifier (const char *);

namespace Rust {
namespace Compile {
namespace handlers {

static tree
make_unsigned_long_tree (unsigned long value)
{
  return build_int_cst (integer_type_node, value);
}

static bool
is_basic_integer_type (TyTy::BaseType *type)
{
  switch (type->get_kind ())
    {
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
      return true;
    default:
      return false;
      break;
    }
}

/**
 * Maybe override the Hir Lookups for the substituions in this context
 */
static void
maybe_override_ctx (TyTy::FnType *fntype)
{
  if (fntype->has_substitutions_defined ())
    fntype->override_context ();
}

static bool
check_for_basic_integer_type (const std::string &intrinsic_str,
			      location_t locus, TyTy::BaseType *type)
{
  auto is_basic_integer = is_basic_integer_type (type);
  if (!is_basic_integer)
    {
      rust_error_at (
	locus,
	"%s intrinsic can only be used with basic integer types (got %qs)",
	intrinsic_str.c_str (), type->get_name ().c_str ());
    }

  return is_basic_integer;
}

/**
 * Items can be forward compiled which means we may not need to invoke this
 * code. We might also have already compiled this generic function as well.
 */
static bool
check_for_cached_intrinsic (Context *ctx, TyTy::FnType *fntype, tree *lookup)
{
  const Resolver::CanonicalPath &canonical_path = fntype->get_ident ().path;
  std::string asm_name = ctx->mangle_item (fntype, canonical_path);
  if (ctx->lookup_function_decl (fntype->get_ty_ref (), lookup,
				 fntype->get_id (), fntype, asm_name))
    {
      return true;
    }

  return false;
}

static tree
compile_intrinsic_function (Context *ctx, TyTy::FnType *fntype)
{
  maybe_override_ctx (fntype);

  const Resolver::CanonicalPath &canonical_path = fntype->get_ident ().path;

  tree compiled_fn_type = TyTyResolveCompile::compile (ctx, fntype);
  std::string ir_symbol_name
    = canonical_path.get () + fntype->subst_as_string ();
  std::string asm_name = ctx->mangle_item (fntype, canonical_path);

  unsigned int flags = 0;
  tree fndecl = Backend::function (compiled_fn_type, ir_symbol_name, asm_name,
				   flags, fntype->get_ident ().locus);

  TREE_PUBLIC (fndecl) = 0;
  TREE_READONLY (fndecl) = 1;
  DECL_ARTIFICIAL (fndecl) = 1;
  DECL_EXTERNAL (fndecl) = 0;
  DECL_DECLARED_INLINE_P (fndecl) = 1;

  return fndecl;
}

/**
 * Compile and setup a function's parameters
 */
static void
compile_fn_params (Context *ctx, TyTy::FnType *fntype, tree fndecl,
		   std::vector<Bvariable *> *compiled_param_variables,
		   std::vector<tree_node *> *compiled_param_types = nullptr)
{
  for (auto &parm : fntype->get_params ())
    {
      auto &referenced_param = parm.get_pattern ();
      auto param_tyty = parm.get_type ();
      auto compiled_param_type = TyTyResolveCompile::compile (ctx, param_tyty);

      location_t param_locus = referenced_param.get_locus ();
      Bvariable *compiled_param_var
	= CompileFnParam::compile (ctx, fndecl, referenced_param,
				   compiled_param_type, param_locus);

      compiled_param_variables->push_back (compiled_param_var);
      if (compiled_param_types)
	compiled_param_types->push_back (compiled_param_type);
    }
}

static void
enter_intrinsic_block (Context *ctx, tree fndecl,
		       const std::vector<Bvariable *> &vars = {})
{
  tree enclosing_scope = NULL_TREE;
  location_t start_location = UNDEF_LOCATION;
  location_t end_location = UNDEF_LOCATION;

  auto block = Backend::block (fndecl, enclosing_scope, vars, start_location,
			       end_location);

  ctx->push_block (block);
}

static void
finalize_intrinsic_block (Context *ctx, tree fndecl)
{
  tree bind_tree = ctx->pop_block ();

  gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);

  DECL_SAVED_TREE (fndecl) = bind_tree;

  ctx->push_function (fndecl);

  DECL_DECLARED_CONSTEXPR_P (fndecl) = 1;
  maybe_save_constexpr_fundef (fndecl);
}

namespace inner {

static std::string
build_atomic_builtin_name (const std::string &prefix, location_t locus,
			   TyTy::BaseType *operand_type)
{
  static const std::map<std::string, std::string> allowed_types = {
    {"i8", "1"},    {"i16", "2"},   {"i32", "4"},   {"i64", "8"},
    {"i128", "16"}, {"isize", "8"}, {"u8", "1"},    {"u16", "2"},
    {"u32", "4"},   {"u64", "8"},   {"u128", "16"}, {"usize", "8"},
  };

  // TODO: Can we maybe get the generic version (atomic_store_n) to work... This
  // would be so much better

  std::string result = "__" + prefix; //  + "n";

  auto type_name = operand_type->get_name ();
  if (type_name == "usize" || type_name == "isize")
    {
      rust_sorry_at (
	locus, "atomics are not yet available for size types (usize, isize)");
      return "";
    }

  if (type_name.at (0) == 'i')
    {
      rust_sorry_at (locus, "atomics are not yet supported for signed "
			    "integer types (i8, i16, i32, i64, i128)");
      return "";
    }

  auto type_size_str = allowed_types.find (type_name);

  if (!check_for_basic_integer_type ("atomic operation", locus, operand_type))
    return "";

  result += type_size_str->second;

  return result;
}

inline tree
unchecked_op (Context *ctx, TyTy::FnType *fntype, tree_code op)
{
  rust_assert (fntype->get_params ().size () == 2);
  rust_assert (fntype->get_num_substitutions () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // setup the params
  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN unchecked_<op> BODY BEGIN

  auto x = Backend::var_expression (param_vars[0], UNDEF_LOCATION);
  auto y = Backend::var_expression (param_vars[1], UNDEF_LOCATION);

  auto *monomorphized_type
    = fntype->get_substs ().at (0).get_param_ty ()->resolve ();

  auto call_locus = ctx->get_mappings ().lookup_location (fntype->get_ref ());
  check_for_basic_integer_type ("unchecked operation", call_locus,
				monomorphized_type);

  auto expr = build2 (op, TREE_TYPE (x), x, y);
  auto return_statement
    = Backend::return_statement (fndecl, expr, UNDEF_LOCATION);

  ctx->add_statement (return_statement);

  // BUILTIN unchecked_<op> BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

inline tree
expect (Context *ctx, TyTy::FnType *fntype, bool likely)
{
  rust_assert (fntype->get_params ().size () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN expect_handler_inner FN BODY BEGIN
  // setup the params
  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);
  tree expr = Backend::var_expression (param_vars[0], UNDEF_LOCATION);
  tree expect_fn_raw = nullptr;
  BuiltinsContext::get ().lookup_simple_builtin ("__builtin_expect",
						 &expect_fn_raw);
  rust_assert (expect_fn_raw);
  auto expect_fn = build_fold_addr_expr_loc (BUILTINS_LOCATION, expect_fn_raw);

  // we need to convert the expression return type to long to match the expected
  // parameter type of __builtin_expect
  auto expect_src = build1 (CONVERT_EXPR, long_integer_type_node, expr);
  auto expect_value
    = make_unsigned_long_tree (static_cast<unsigned long> (likely));

  auto expect_call
    = Backend::call_expression (expect_fn, {expect_src, expect_value}, nullptr,
				BUILTINS_LOCATION);
  // the return value also needs to be casted (to bool)
  auto expect_call_bool = build1 (CONVERT_EXPR, boolean_type_node, expect_call);
  auto return_statement
    = Backend::return_statement (fndecl, expect_call_bool, BUILTINS_LOCATION);
  ctx->add_statement (return_statement);
  // BUILTIN expect_handler_inner FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
try_handler (Context *ctx, TyTy::FnType *fntype, bool is_new_api)
{
  rust_assert (fntype->get_params ().size () == 3);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;
  auto fndecl = compile_intrinsic_function (ctx, fntype);

  enter_intrinsic_block (ctx, fndecl);

  // The following tricks are needed to make sure the try-catch blocks are not
  // optimized away
  TREE_READONLY (fndecl) = 0;
  DECL_DISREGARD_INLINE_LIMITS (fndecl) = 1;
  DECL_ATTRIBUTES (fndecl) = tree_cons (get_identifier ("always_inline"),
					NULL_TREE, DECL_ATTRIBUTES (fndecl));

  // BUILTIN try_handler FN BODY BEGIN
  // setup the params
  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);
  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;
  tree enclosing_scope = NULL_TREE;

  bool panic_is_abort = Session::get_instance ().options.get_panic_strategy ()
			== CompileOptions::PanicStrategy::Abort;
  tree try_fn = Backend::var_expression (param_vars[0], UNDEF_LOCATION);
  tree user_data = Backend::var_expression (param_vars[1], UNDEF_LOCATION);
  tree catch_fn = Backend::var_expression (param_vars[2], UNDEF_LOCATION);
  tree normal_return_stmt = NULL_TREE;
  tree error_return_stmt = NULL_TREE;
  tree try_call = Backend::call_expression (try_fn, {user_data}, nullptr,
					    BUILTINS_LOCATION);
  tree catch_call = NULL_TREE;
  tree try_block = Backend::block (fndecl, enclosing_scope, {}, UNDEF_LOCATION,
				   UNDEF_LOCATION);

  if (is_new_api)
    {
      auto ret_type = TyTyResolveCompile::get_unit_type (ctx);
      auto ret_expr = Backend::constructor_expression (ret_type, false, {}, -1,
						       UNDEF_LOCATION);
      normal_return_stmt
	= Backend::return_statement (fndecl, ret_expr, BUILTINS_LOCATION);
      error_return_stmt
	= Backend::return_statement (fndecl, ret_expr, BUILTINS_LOCATION);
    }
  else
    {
      normal_return_stmt = Backend::return_statement (fndecl, integer_zero_node,
						      BUILTINS_LOCATION);
      error_return_stmt = Backend::return_statement (fndecl, integer_one_node,
						     BUILTINS_LOCATION);
    }
  Backend::block_add_statements (try_block,
				 std::vector<tree>{try_call,
						   normal_return_stmt});
  if (panic_is_abort)
    {
      // skip building the try-catch construct
      ctx->add_statement (try_block);
      finalize_intrinsic_block (ctx, fndecl);
      return fndecl;
    }

  tree eh_pointer
    = build_call_expr (builtin_decl_explicit (BUILT_IN_EH_POINTER), 1,
		       integer_zero_node);
  catch_call = Backend::call_expression (catch_fn, {user_data, eh_pointer},
					 NULL_TREE, BUILTINS_LOCATION);

  tree catch_block = Backend::block (fndecl, enclosing_scope, {},
				     UNDEF_LOCATION, UNDEF_LOCATION);
  Backend::block_add_statements (catch_block,
				 std::vector<tree>{catch_call,
						   error_return_stmt});
  // emulate what cc1plus is doing for C++ try-catch
  tree inner_eh_construct
    = Backend::exception_handler_statement (catch_call, NULL_TREE,
					    error_return_stmt,
					    BUILTINS_LOCATION);
  // TODO(liushuyu): eh_personality needs to be implemented as a runtime thing
  auto eh_construct
    = Backend::exception_handler_statement (try_block, inner_eh_construct,
					    NULL_TREE, BUILTINS_LOCATION);
  ctx->add_statement (eh_construct);
  // BUILTIN try_handler FN BODY END
  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

/**
 * pub fn wrapping_{add, sub, mul}<T>(lhs: T, rhs: T) -> T;
 */
tree
wrapping_op (Context *ctx, TyTy::FnType *fntype, tree_code op)
{
  // wrapping_<op> intrinsics have two parameter
  rust_assert (fntype->get_params ().size () == 2);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // setup the params
  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto &lhs_param = param_vars.at (0);
  auto &rhs_param = param_vars.at (1);

  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN wrapping_<op> FN BODY BEGIN
  auto lhs = Backend::var_expression (lhs_param, UNDEF_LOCATION);
  auto rhs = Backend::var_expression (rhs_param, UNDEF_LOCATION);

  // Operations are always wrapping in Rust, as we have -fwrapv enabled by
  // default. The difference between a wrapping_{add, sub, mul} and a regular
  // arithmetic operation is that these intrinsics do not panic - they always
  // carry over.
  auto wrap_expr = build2 (op, TREE_TYPE (lhs), lhs, rhs);

  auto return_statement
    = Backend::return_statement (fndecl, wrap_expr, UNDEF_LOCATION);
  ctx->add_statement (return_statement);
  // BUILTIN wrapping_<op> FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

/**
 * pub fn add_with_overflow<T>(x: T, y: T) -> (T, bool);
 */
tree
op_with_overflow (Context *ctx, TyTy::FnType *fntype, tree_code op)
{
  // wrapping_<op> intrinsics have two parameter
  rust_assert (fntype->get_params ().size () == 2);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // setup the params
  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto &x_param = param_vars.at (0);
  auto &y_param = param_vars.at (1);

  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  rust_assert (fntype->get_num_substitutions () == 1);
  auto &param_mapping = fntype->get_substs ().at (0);
  const auto param_tyty = param_mapping.get_param_ty ();
  auto resolved_tyty = param_tyty->resolve ();
  tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, resolved_tyty);

  // this should match y as well or we can take it from the TyTy structure
  tree tmp_stmt = error_mark_node;
  Bvariable *result_variable
    = Backend::temporary_variable (fndecl, NULL_TREE, template_parameter_type,
				   NULL_TREE, true /*address_is_taken*/,
				   UNDEF_LOCATION, &tmp_stmt);
  Bvariable *bool_variable
    = Backend::temporary_variable (fndecl, NULL_TREE, boolean_type_node,
				   NULL_TREE, true /*address_is_taken*/,
				   UNDEF_LOCATION, &tmp_stmt);

  enter_intrinsic_block (ctx, fndecl, {result_variable, bool_variable});

  // BUILTIN op_with_overflow FN BODY BEGIN
  auto x = Backend::var_expression (x_param, UNDEF_LOCATION);
  auto y = Backend::var_expression (y_param, UNDEF_LOCATION);

  tree overflow_builtin = error_mark_node;
  switch (op)
    {
    case PLUS_EXPR:
      BuiltinsContext::get ().lookup_simple_builtin ("__builtin_add_overflow",
						     &overflow_builtin);
      break;

    case MINUS_EXPR:
      BuiltinsContext::get ().lookup_simple_builtin ("__builtin_sub_overflow",
						     &overflow_builtin);
      break;

    case MULT_EXPR:
      BuiltinsContext::get ().lookup_simple_builtin ("__builtin_mul_overflow",
						     &overflow_builtin);
      break;

    default:
      rust_unreachable ();
      break;
    }
  rust_assert (overflow_builtin != error_mark_node);

  tree bool_decl = bool_variable->get_tree (BUILTINS_LOCATION);
  tree result_decl = result_variable->get_tree (BUILTINS_LOCATION);
  tree result_ref = build_fold_addr_expr_loc (BUILTINS_LOCATION, result_decl);

  tree builtin_call = build_call_expr_loc (BUILTINS_LOCATION, overflow_builtin,
					   3, x, y, result_ref);

  tree overflow_assignment
    = Backend::assignment_statement (bool_decl, builtin_call,
				     BUILTINS_LOCATION);

  ctx->add_statement (overflow_assignment);

  std::vector<tree> vals = {result_decl, bool_decl};
  tree tuple_type = TREE_TYPE (DECL_RESULT (fndecl));
  tree result_expr = Backend::constructor_expression (tuple_type, false, vals,
						      -1, UNDEF_LOCATION);

  auto return_statement
    = Backend::return_statement (fndecl, result_expr, UNDEF_LOCATION);
  ctx->add_statement (return_statement);

  // BUILTIN wrapping_<op> FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

/**
 * fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
 * fn copy<T>(src: *const T, dst: *mut T, count: usize);
 */
tree
copy (Context *ctx, TyTy::FnType *fntype, bool overlaps)
{
  rust_assert (fntype->get_params ().size () == 3);
  rust_assert (fntype->get_num_substitutions () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // Most intrinsic functions are pure - not `copy_nonoverlapping` and `copy`
  TREE_READONLY (fndecl) = 0;
  TREE_SIDE_EFFECTS (fndecl) = 1;

  // setup the params
  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN copy_nonoverlapping BODY BEGIN

  auto src = Backend::var_expression (param_vars[0], UNDEF_LOCATION);
  auto dst = Backend::var_expression (param_vars[1], UNDEF_LOCATION);
  auto count = Backend::var_expression (param_vars[2], UNDEF_LOCATION);

  // We want to create the following statement
  // memcpy(dst, src, size_of::<T>());
  // so
  // memcpy(dst, src, size_expr);

  auto *resolved_ty = fntype->get_substs ().at (0).get_param_ty ()->resolve ();
  auto param_type = TyTyResolveCompile::compile (ctx, resolved_ty);

  tree size_expr
    = build2 (MULT_EXPR, size_type_node, TYPE_SIZE_UNIT (param_type), count);

  tree memcpy_raw = nullptr;
  BuiltinsContext::get ().lookup_simple_builtin (overlaps ? "__builtin_memmove"
							  : "__builtin_memcpy",
						 &memcpy_raw);
  rust_assert (memcpy_raw);
  auto memcpy = build_fold_addr_expr_loc (UNKNOWN_LOCATION, memcpy_raw);

  auto copy_call = Backend::call_expression (memcpy, {dst, src, size_expr},
					     nullptr, UNDEF_LOCATION);

  ctx->add_statement (copy_call);

  // BUILTIN copy_nonoverlapping BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
atomic_store (Context *ctx, TyTy::FnType *fntype, int ordering)
{
  rust_assert (fntype->get_params ().size () == 2);
  rust_assert (fntype->get_num_substitutions () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // Most intrinsic functions are pure but not the atomic ones
  TREE_READONLY (fndecl) = 0;
  TREE_SIDE_EFFECTS (fndecl) = 1;

  // setup the params
  std::vector<Bvariable *> param_vars;
  std::vector<tree> types;
  compile_fn_params (ctx, fntype, fndecl, &param_vars, &types);

  auto ok = Backend::function_set_parameters (fndecl, param_vars);
  rust_assert (ok);

  enter_intrinsic_block (ctx, fndecl);

  auto dst = Backend::var_expression (param_vars[0], UNDEF_LOCATION);
  TREE_READONLY (dst) = 0;

  auto value = Backend::var_expression (param_vars[1], UNDEF_LOCATION);
  auto memorder = make_unsigned_long_tree (ordering);

  auto monomorphized_type
    = fntype->get_substs ()[0].get_param_ty ()->resolve ();

  auto call_locus = ctx->get_mappings ().lookup_location (fntype->get_ref ());
  auto builtin_name = build_atomic_builtin_name ("atomic_store_", call_locus,
						 monomorphized_type);
  if (builtin_name.empty ())
    return error_mark_node;

  tree atomic_store_raw = nullptr;
  BuiltinsContext::get ().lookup_simple_builtin (builtin_name,
						 &atomic_store_raw);
  rust_assert (atomic_store_raw);

  auto atomic_store
    = build_fold_addr_expr_loc (UNKNOWN_LOCATION, atomic_store_raw);

  auto store_call
    = Backend::call_expression (atomic_store, {dst, value, memorder}, nullptr,
				UNDEF_LOCATION);
  TREE_READONLY (store_call) = 0;
  TREE_SIDE_EFFECTS (store_call) = 1;

  ctx->add_statement (store_call);
  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
atomic_load (Context *ctx, TyTy::FnType *fntype, int ordering)
{
  rust_assert (fntype->get_params ().size () == 1);
  rust_assert (fntype->get_num_substitutions () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // Most intrinsic functions are pure but not the atomic ones
  // FIXME: Is atomic_load_* pure? Feels like it shouldn't so
  TREE_READONLY (fndecl) = 0;
  TREE_SIDE_EFFECTS (fndecl) = 1;

  // setup the params
  std::vector<Bvariable *> param_vars;
  std::vector<tree> types;
  compile_fn_params (ctx, fntype, fndecl, &param_vars, &types);

  auto ok = Backend::function_set_parameters (fndecl, param_vars);
  rust_assert (ok);

  enter_intrinsic_block (ctx, fndecl);

  auto src = Backend::var_expression (param_vars[0], UNDEF_LOCATION);
  auto memorder = make_unsigned_long_tree (ordering);

  auto monomorphized_type
    = fntype->get_substs ()[0].get_param_ty ()->resolve ();

  auto builtin_name
    = build_atomic_builtin_name ("atomic_load_", fntype->get_locus (),
				 monomorphized_type);
  if (builtin_name.empty ())
    return error_mark_node;

  tree atomic_load_raw = nullptr;
  BuiltinsContext::get ().lookup_simple_builtin (builtin_name,
						 &atomic_load_raw);
  rust_assert (atomic_load_raw);

  auto atomic_load
    = build_fold_addr_expr_loc (UNKNOWN_LOCATION, atomic_load_raw);

  auto load_call = Backend::call_expression (atomic_load, {src, memorder},
					     nullptr, UNDEF_LOCATION);
  auto return_statement
    = Backend::return_statement (fndecl, load_call, UNDEF_LOCATION);

  TREE_READONLY (load_call) = 0;
  TREE_SIDE_EFFECTS (load_call) = 1;

  ctx->add_statement (return_statement);

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

// Shared inner implementation for ctlz and ctlz_nonzero.
//
// nonzero=false → ctlz: ctlz(0) is well-defined in Rust and must return
//   bit_size, but __builtin_clz*(0) is undefined behaviour in C, so an
//   explicit arg==0 guard is emitted.
//
// nonzero=true → ctlz_nonzero: the caller guarantees arg != 0 (passing 0
//   is immediate UB in Rust), so the zero guard is omitted entirely.
static tree
ctlz_handler (Context *ctx, TyTy::FnType *fntype, bool nonzero)
{
  rust_assert (fntype->get_params ().size () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto arg_param = param_vars.at (0);
  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  rust_assert (fntype->get_num_substitutions () == 1);
  auto *monomorphized_type
    = fntype->get_substs ().at (0).get_param_ty ()->resolve ();
  auto call_locus = ctx->get_mappings ().lookup_location (fntype->get_ref ());
  if (!check_for_basic_integer_type ("ctlz", call_locus, monomorphized_type))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN ctlz FN BODY BEGIN
  auto locus = fntype->get_locus ();
  auto arg_expr = Backend::var_expression (arg_param, locus);
  tree arg_type = TREE_TYPE (arg_expr);
  unsigned bit_size = TYPE_PRECISION (arg_type);

  // Convert signed types to their same-width unsigned equivalent before
  // widening.  Without this, widening a signed type sign-extends it.
  // For example, i8(-1) = 0xFF widened to u32 gives 0xFFFFFFFF, so
  // __builtin_clz(0xFFFFFFFF) = 0, then 0 - diff(24) = -24.
  // Converting to u8 first gives 0xFF → 0x000000FF (zero-extended), so
  // __builtin_clz(0x000000FF) = 24, then 24 - 24 = 0.
  tree unsigned_type
    = !TYPE_UNSIGNED (arg_type) ? unsigned_type_for (arg_type) : arg_type;
  tree unsigned_arg = fold_convert (unsigned_type, arg_expr);

  // Pick the narrowest GCC clz builtin whose operand type is wide enough to
  // hold bit_size bits.  diff records how many extra leading zeros the builtin
  // will count due to the width difference and is subtracted from the result.
  //
  // Example: ctlz(1u8) bit_size=8, int_prec=32, diff=24.
  //   __builtin_clz(1u) returns 31 (counts from bit 31 down to bit 0).
  //   31 - 24 = 7, which is the correct answer for an 8-bit value.
  //
  // TODO: 128-bit integers are not yet handled.
  unsigned int_prec = TYPE_PRECISION (unsigned_type_node);
  unsigned long_prec = TYPE_PRECISION (long_unsigned_type_node);
  unsigned longlong_prec = TYPE_PRECISION (long_long_unsigned_type_node);

  const char *builtin_name = nullptr;
  tree cast_type = NULL_TREE;
  int diff = 0;

  if (bit_size <= int_prec)
    {
      // Fits in unsigned int: covers 8/16/32-bit integers on most targets.
      builtin_name = "__builtin_clz";
      cast_type = unsigned_type_node;
      diff = static_cast<int> (int_prec - bit_size);
    }
  else if (bit_size <= long_prec)
    {
      // Fits in unsigned long but not unsigned int.
      builtin_name = "__builtin_clzl";
      cast_type = long_unsigned_type_node;
      diff = static_cast<int> (long_prec - bit_size);
    }
  else if (bit_size <= longlong_prec)
    {
      // Fits in unsigned long long but not unsigned long.
      builtin_name = "__builtin_clzll";
      cast_type = long_long_unsigned_type_node;
      diff = static_cast<int> (longlong_prec - bit_size);
    }
  else
    {
      rust_sorry_at (locus, "ctlz for %u-bit integers is not yet implemented",
		     bit_size);
      return error_mark_node;
    }

  // Widen the unsigned arg to the chosen builtin's operand type, call it,
  // then subtract the padding bits.  diff == 0 means the Rust type exactly
  // matches the builtin's operand width, so the subtraction is skipped.
  tree call_arg = fold_convert (cast_type, unsigned_arg);

  tree builtin_decl = error_mark_node;
  BuiltinsContext::get ().lookup_simple_builtin (builtin_name, &builtin_decl);
  rust_assert (builtin_decl != error_mark_node);

  tree builtin_fn = build_fold_addr_expr_loc (locus, builtin_decl);
  tree clz_expr
    = Backend::call_expression (builtin_fn, {call_arg}, nullptr, locus);

  if (diff > 0)
    {
      tree diff_cst = build_int_cst (integer_type_node, diff);
      clz_expr
	= fold_build2 (MINUS_EXPR, integer_type_node, clz_expr, diff_cst);
    }

  clz_expr = fold_convert (uint32_type_node, clz_expr);

  tree final_expr;
  if (!nonzero)
    {
      // ctlz(0) must return bit_size per the Rust reference.
      // We cannot pass 0 to __builtin_clz* (UB), so emit:
      //   arg == 0 ? bit_size : clz_expr
      tree zero = build_int_cst (arg_type, 0);
      tree cmp = fold_build2 (EQ_EXPR, boolean_type_node, arg_expr, zero);
      tree width_cst = build_int_cst (uint32_type_node, bit_size);
      final_expr
	= fold_build3 (COND_EXPR, uint32_type_node, cmp, width_cst, clz_expr);
    }
  else
    {
      // ctlz_nonzero: arg != 0 is guaranteed by the caller, no guard needed.
      final_expr = clz_expr;
    }

  tree result = fold_convert (TREE_TYPE (DECL_RESULT (fndecl)), final_expr);
  auto return_stmt = Backend::return_statement (fndecl, result, locus);
  ctx->add_statement (return_stmt);
  // BUILTIN ctlz FN BODY END

  finalize_intrinsic_block (ctx, fndecl);
  return fndecl;
}

// Shared inner implementation for cttz and cttz_nonzero.
//
// nonzero=false → cttz: cttz(0) is well-defined in Rust and must return
//   bit_size, but __builtin_ctz*(0) is undefined behaviour in C, so an
//   explicit arg==0 guard is emitted.
//
// nonzero=true → cttz_nonzero: the caller guarantees arg != 0 (passing 0
//   is immediate UB in Rust), so the zero guard is omitted entirely.
static tree
cttz_handler (Context *ctx, TyTy::FnType *fntype, bool nonzero)
{
  rust_assert (fntype->get_params ().size () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto arg_param = param_vars.at (0);
  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  rust_assert (fntype->get_num_substitutions () == 1);
  auto *monomorphized_type
    = fntype->get_substs ().at (0).get_param_ty ()->resolve ();
  auto call_locus = ctx->get_mappings ().lookup_location (fntype->get_ref ());
  if (!check_for_basic_integer_type ("cttz", call_locus, monomorphized_type))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN cttz FN BODY BEGIN
  auto locus = fntype->get_locus ();
  auto arg_expr = Backend::var_expression (arg_param, locus);
  tree arg_type = TREE_TYPE (arg_expr);
  unsigned bit_size = TYPE_PRECISION (arg_type);

  // Convert signed types to their same-width unsigned equivalent before
  // widening.  For cttz this is not strictly required for correctness (sign
  // extension fills high bits with 1s, which does not alter the trailing-zero
  // count at the low end), but it avoids relying on signed-integer
  // representations and keeps the approach consistent with ctlz.
  tree unsigned_type
    = !TYPE_UNSIGNED (arg_type) ? unsigned_type_for (arg_type) : arg_type;
  tree unsigned_arg = fold_convert (unsigned_type, arg_expr);

  // Pick the narrowest GCC ctz builtin whose operand type is wide enough to
  // hold bit_size bits.  Unlike ctlz, no diff adjustment is needed: widening
  // a value zero-extends it (fills the added high bits with 0s), which does
  // not introduce new trailing zeros at the low end.
  //
  // Example: cttz(0b00001000_u8) = 3
  //   Widened to u32: 0x00000008.  __builtin_ctz(0x00000008) = 3.
  //
  // TODO: 128-bit integers are not yet handled.
  unsigned int_prec = TYPE_PRECISION (unsigned_type_node);
  unsigned long_prec = TYPE_PRECISION (long_unsigned_type_node);
  unsigned longlong_prec = TYPE_PRECISION (long_long_unsigned_type_node);

  const char *builtin_name = nullptr;
  tree cast_type = NULL_TREE;

  if (bit_size <= int_prec)
    {
      // Fits in unsigned int: covers 8/16/32-bit integers on most targets.
      builtin_name = "__builtin_ctz";
      cast_type = unsigned_type_node;
    }
  else if (bit_size <= long_prec)
    {
      // Fits in unsigned long but not unsigned int.
      builtin_name = "__builtin_ctzl";
      cast_type = long_unsigned_type_node;
    }
  else if (bit_size <= longlong_prec)
    {
      // Fits in unsigned long long but not unsigned long.
      builtin_name = "__builtin_ctzll";
      cast_type = long_long_unsigned_type_node;
    }
  else
    {
      rust_sorry_at (locus, "cttz for %u-bit integers is not yet implemented",
		     bit_size);
      return error_mark_node;
    }

  tree call_arg = fold_convert (cast_type, unsigned_arg);

  tree builtin_decl = error_mark_node;
  BuiltinsContext::get ().lookup_simple_builtin (builtin_name, &builtin_decl);
  rust_assert (builtin_decl != error_mark_node);

  tree builtin_fn = build_fold_addr_expr_loc (locus, builtin_decl);
  tree ctz_expr
    = Backend::call_expression (builtin_fn, {call_arg}, nullptr, locus);

  ctz_expr = fold_convert (uint32_type_node, ctz_expr);

  tree final_expr;
  if (!nonzero)
    {
      // cttz(0) must return bit_size per the Rust reference.
      // We cannot pass 0 to __builtin_ctz* (UB), so emit:
      //   arg == 0 ? bit_size : ctz_expr
      tree zero = build_int_cst (arg_type, 0);
      tree cmp = fold_build2 (EQ_EXPR, boolean_type_node, arg_expr, zero);
      tree width_cst = build_int_cst (uint32_type_node, bit_size);
      final_expr
	= fold_build3 (COND_EXPR, uint32_type_node, cmp, width_cst, ctz_expr);
    }
  else
    {
      // cttz_nonzero: arg != 0 is guaranteed by the caller, no guard needed.
      final_expr = ctz_expr;
    }

  tree result = fold_convert (TREE_TYPE (DECL_RESULT (fndecl)), final_expr);
  auto return_stmt = Backend::return_statement (fndecl, result, locus);
  ctx->add_statement (return_stmt);
  // BUILTIN cttz FN BODY END

  finalize_intrinsic_block (ctx, fndecl);
  return fndecl;
}

} // namespace inner

const HandlerBuilder
op_with_overflow (tree_code op)
{
  return [op] (Context *ctx, TyTy::FnType *fntype, location_t) {
    return inner::op_with_overflow (ctx, fntype, op);
  };
}

tree
rotate_left (Context *ctx, TyTy::FnType *fntype, location_t)
{
  return handlers::rotate (ctx, fntype, LROTATE_EXPR);
}

tree
rotate_right (Context *ctx, TyTy::FnType *fntype, location_t)
{
  return handlers::rotate (ctx, fntype, RROTATE_EXPR);
}

const HandlerBuilder
wrapping_op (tree_code op)
{
  return [op] (Context *ctx, TyTy::FnType *fntype, location_t) {
    return inner::wrapping_op (ctx, fntype, op);
  };
}

HandlerBuilder
atomic_store (int ordering)
{
  return [ordering] (Context *ctx, TyTy::FnType *fntype, location_t) {
    return inner::atomic_store (ctx, fntype, ordering);
  };
}

HandlerBuilder
atomic_load (int ordering)
{
  return [ordering] (Context *ctx, TyTy::FnType *fntype, location_t) {
    return inner::atomic_load (ctx, fntype, ordering);
  };
}

const HandlerBuilder
unchecked_op (tree_code op)
{
  return [op] (Context *ctx, TyTy::FnType *fntype, location_t) {
    return inner::unchecked_op (ctx, fntype, op);
  };
}

const HandlerBuilder
copy (bool overlaps)
{
  return [overlaps] (Context *ctx, TyTy::FnType *fntype, location_t) {
    return inner::copy (ctx, fntype, overlaps);
  };
}

const HandlerBuilder
expect (bool likely)
{
  return [likely] (Context *ctx, TyTy::FnType *fntype, location_t) {
    return inner::expect (ctx, fntype, likely);
  };
}

const HandlerBuilder
try_handler (bool is_new_api)
{
  return [is_new_api] (Context *ctx, TyTy::FnType *fntype, location_t) {
    return inner::try_handler (ctx, fntype, is_new_api);
  };
}

tree
sorry (Context *ctx, TyTy::FnType *fntype, location_t)
{
  rust_sorry_at (fntype->get_locus (), "intrinsic %qs is not yet implemented",
		 fntype->get_identifier ().c_str ());

  return error_mark_node;
}

tree
assume (Context *ctx, TyTy::FnType *fntype, location_t)
{
  // TODO: make sure this is actually helping the compiler optimize

  rust_assert (fntype->get_params ().size () == 1);
  rust_assert (fntype->param_at (0).get_type ()->get_kind ()
	       == TyTy::TypeKind::BOOL);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // TODO: make sure these are necessary
  TREE_READONLY (fndecl) = 0;
  DECL_DISREGARD_INLINE_LIMITS (fndecl) = 1;
  DECL_ATTRIBUTES (fndecl) = tree_cons (get_identifier ("always_inline"),
					NULL_TREE, DECL_ATTRIBUTES (fndecl));

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN assume FN BODY BEGIN

  tree val = Backend::var_expression (param_vars[0], UNDEF_LOCATION);

  tree assume_expr = build_call_expr_internal_loc (UNDEF_LOCATION, IFN_ASSUME,
						   void_type_node, 1, val);
  TREE_SIDE_EFFECTS (assume_expr) = 1;

  ctx->add_statement (assume_expr);
  // BUILTIN assume FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
discriminant_value (Context *ctx, TyTy::FnType *fntype, location_t)
{
  rust_assert (fntype->get_params ().size () == 1);
  rust_assert (fntype->has_substitutions ());
  rust_assert (fntype->get_num_type_params () == 1);
  auto &mapping = fntype->get_substs ().at (0);
  auto param_ty = mapping.get_param_ty ();
  rust_assert (param_ty->can_resolve ());
  auto resolved = param_ty->resolve ();

  TyTy::BaseType *return_type = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_builtin ("isize", &return_type);
  rust_assert (ok);

  bool is_adt = resolved->is<TyTy::ADTType> ();
  bool is_enum = false;
  if (is_adt)
    {
      const auto &adt = *static_cast<TyTy::ADTType *> (resolved);
      auto *repr = adt.get_repr_options ().repr;
      if (repr != nullptr)
	return_type = repr;
      is_enum = adt.is_enum ();
    }

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN disriminant_value FN BODY BEGIN

  tree result = integer_zero_node;
  if (is_enum)
    {
      tree val = Backend::var_expression (param_vars[0], UNDEF_LOCATION);
      tree deref = build_fold_indirect_ref_loc (UNKNOWN_LOCATION, val);
      result = Backend::struct_field_expression (deref, 0, UNKNOWN_LOCATION);
    }

  auto return_statement
    = Backend::return_statement (fndecl, result, BUILTINS_LOCATION);
  ctx->add_statement (return_statement);

  // BUILTIN disriminant_value FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
variant_count (Context *ctx, TyTy::FnType *fntype, location_t)
{
  rust_assert (fntype->get_num_type_params () == 1);
  auto &mapping = fntype->get_substs ().at (0);
  auto param_ty = mapping.get_param_ty ();
  rust_assert (param_ty->can_resolve ());
  auto resolved = param_ty->resolve ();

  size_t variant_count = 0;
  bool is_adt = resolved->is<TyTy::ADTType> ();
  if (is_adt)
    {
      const auto &adt = *static_cast<TyTy::ADTType *> (resolved);
      variant_count = adt.number_of_variants ();
    }

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN disriminant_value FN BODY BEGIN
  tree result_decl = DECL_RESULT (fndecl);
  tree type = TREE_TYPE (result_decl);

  mpz_t ival;
  mpz_init_set_ui (ival, variant_count);
  tree result = wide_int_to_tree (type, wi::from_mpz (type, ival, true));
  mpz_clear (ival);

  auto return_statement
    = Backend::return_statement (fndecl, result, BUILTINS_LOCATION);
  ctx->add_statement (return_statement);

  // BUILTIN disriminant_value FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
move_val_init (Context *ctx, TyTy::FnType *fntype, location_t)
{
  rust_assert (fntype->get_params ().size () == 2);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // Most intrinsic functions are pure - not `move_val_init`
  TREE_READONLY (fndecl) = 0;
  TREE_SIDE_EFFECTS (fndecl) = 1;

  // get the template parameter type tree fn size_of<T>();
  rust_assert (fntype->get_num_substitutions () == 1);
  auto &param_mapping = fntype->get_substs ().at (0);
  auto param_tyty = param_mapping.get_param_ty ();
  auto resolved_tyty = param_tyty->resolve ();
  tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, resolved_tyty);

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN size_of FN BODY BEGIN

  tree dst = Backend::var_expression (param_vars[0], UNDEF_LOCATION);
  tree src = Backend::var_expression (param_vars[1], UNDEF_LOCATION);
  tree size = TYPE_SIZE_UNIT (template_parameter_type);

  tree memcpy_builtin = error_mark_node;
  BuiltinsContext::get ().lookup_simple_builtin ("__builtin_memcpy",
						 &memcpy_builtin);
  rust_assert (memcpy_builtin != error_mark_node);

  src = build_fold_addr_expr_loc (BUILTINS_LOCATION, src);
  tree memset_call = build_call_expr_loc (BUILTINS_LOCATION, memcpy_builtin, 3,
					  dst, src, size);

  ctx->add_statement (memset_call);
  // BUILTIN size_of FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
uninit (Context *ctx, TyTy::FnType *fntype, location_t)
{
  // uninit has _zero_ parameters its parameter is the generic one
  rust_assert (fntype->get_params ().size () == 0);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // Most intrinsic functions are pure - not `uninit_handler`
  TREE_READONLY (fndecl) = 0;
  TREE_SIDE_EFFECTS (fndecl) = 1;

  // get the template parameter type tree fn uninit<T>();
  rust_assert (fntype->get_num_substitutions () == 1);
  auto &param_mapping = fntype->get_substs ().at (0);
  const auto param_tyty = param_mapping.get_param_ty ();
  auto resolved_tyty = param_tyty->resolve ();
  tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, resolved_tyty);

  // result temporary
  tree dst_type = TREE_TYPE (DECL_RESULT (fndecl));
  rust_assert (TYPE_SIZE_UNIT (template_parameter_type)
	       == TYPE_SIZE_UNIT (dst_type));

  tree tmp_stmt = error_mark_node;
  Bvariable *bvar
    = Backend::temporary_variable (fndecl, NULL_TREE, dst_type, NULL_TREE,
				   true /*address_is_taken*/, UNDEF_LOCATION,
				   &tmp_stmt);

  enter_intrinsic_block (ctx, fndecl, {bvar});

  // BUILTIN size_of FN BODY BEGIN

  tree memset_builtin = error_mark_node;
  BuiltinsContext::get ().lookup_simple_builtin ("__builtin_memset",
						 &memset_builtin);
  rust_assert (memset_builtin != error_mark_node);

  // call memset with 0x01 and size of the thing see
  // https://github.com/Rust-GCC/gccrs/issues/1899

  tree dst = bvar->get_tree (BUILTINS_LOCATION);
  tree dst_addr = build_fold_addr_expr_loc (BUILTINS_LOCATION, dst);
  tree constant_byte = build_int_cst (integer_type_node, 0x01);
  tree size_expr = TYPE_SIZE_UNIT (template_parameter_type);

  tree memset_call = build_call_expr_loc (BUILTINS_LOCATION, memset_builtin, 3,
					  dst_addr, constant_byte, size_expr);
  ctx->add_statement (memset_call);

  auto return_statement
    = Backend::return_statement (fndecl, dst, UNDEF_LOCATION);
  ctx->add_statement (return_statement);
  // BUILTIN size_of FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
prefetch_read_data (Context *ctx, TyTy::FnType *fntype, location_t)
{
  return prefetch_data (ctx, fntype, Prefetch::Read);
}
tree
prefetch_write_data (Context *ctx, TyTy::FnType *fntype, location_t)
{
  return prefetch_data (ctx, fntype, Prefetch::Write);
}

tree
prefetch_data (Context *ctx, TyTy::FnType *fntype, Prefetch kind)
{
  rust_assert (fntype->get_params ().size () == 2);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // prefetching isn't pure and shouldn't be discarded after GIMPLE
  TREE_READONLY (fndecl) = 0;
  TREE_SIDE_EFFECTS (fndecl) = 1;

  std::vector<Bvariable *> args;
  compile_fn_params (ctx, fntype, fndecl, &args);

  if (!Backend::function_set_parameters (fndecl, args))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  auto addr = Backend::var_expression (args[0], UNDEF_LOCATION);

  // The core library technically allows you to pass any i32 value as a
  // locality, but LLVM will then complain if the value cannot be constant
  // evaluated. For now, we ignore the locality argument and instead always
  // pass `3` (the most restrictive value). This allows us to still have
  // prefetch behavior, just not as granular as expected. In future Rust
  // versions, we hope that prefetch intrinsics will be split up according to
  // locality, similarly to atomic intrinsics.
  // The solution is to try and perform constant folding for the locality
  // argument, or instead of creating a new function definition, modify the call
  // site directly This has the bad side-effect of creating warnings about
  // `unused name - locality`, which we hack away here:
  // TODO: Take care of handling locality properly
  Backend::var_expression (args[1], UNDEF_LOCATION);

  auto rw_flag = make_unsigned_long_tree (kind == Prefetch::Write ? 1 : 0);

  auto prefetch_raw = NULL_TREE;
  auto ok = BuiltinsContext::get ().lookup_simple_builtin ("__builtin_prefetch",
							   &prefetch_raw);
  rust_assert (ok);
  auto prefetch = build_fold_addr_expr_loc (UNKNOWN_LOCATION, prefetch_raw);

  auto prefetch_call = Backend::call_expression (prefetch,
						 {addr, rw_flag,
						  // locality arg
						  make_unsigned_long_tree (3)},
						 nullptr, UNDEF_LOCATION);

  TREE_READONLY (prefetch_call) = 0;
  TREE_SIDE_EFFECTS (prefetch_call) = 1;

  ctx->add_statement (prefetch_call);

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
rotate (Context *ctx, TyTy::FnType *fntype, tree_code op)
{
  // rotate intrinsic has two parameter
  rust_assert (fntype->get_params ().size () == 2);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // setup the params
  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto &x_param = param_vars.at (0);
  auto &y_param = param_vars.at (1);
  rust_assert (param_vars.size () == 2);
  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN rotate FN BODY BEGIN
  tree x = Backend::var_expression (x_param, UNDEF_LOCATION);
  tree y = Backend::var_expression (y_param, UNDEF_LOCATION);
  tree rotate_expr
    = fold_build2_loc (BUILTINS_LOCATION, op, TREE_TYPE (x), x, y);
  auto return_statement
    = Backend::return_statement (fndecl, rotate_expr, UNDEF_LOCATION);
  ctx->add_statement (return_statement);
  // BUILTIN rotate FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
transmute (Context *ctx, TyTy::FnType *fntype, location_t)
{
  // transmute intrinsic has one parameter
  rust_assert (fntype->get_params ().size () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  std::vector<Bvariable *> param_vars;
  std::vector<tree_node *> compiled_types;
  compile_fn_params (ctx, fntype, fndecl, &param_vars, &compiled_types);

  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  // param to convert
  Bvariable *convert_me_param = param_vars.at (0);
  tree convert_me_expr
    = Backend::var_expression (convert_me_param, UNDEF_LOCATION);

  // check for transmute pre-conditions
  tree target_type_expr = TREE_TYPE (DECL_RESULT (fndecl));
  tree source_type_expr = compiled_types.at (0);
  tree target_size_expr = TYPE_SIZE (target_type_expr);
  tree source_size_expr = TYPE_SIZE (source_type_expr);
  // for some reason, unit types and other zero-sized types return NULL for the
  // size expressions
  unsigned HOST_WIDE_INT target_size
    = target_size_expr ? TREE_INT_CST_LOW (target_size_expr) : 0;
  unsigned HOST_WIDE_INT source_size
    = source_size_expr ? TREE_INT_CST_LOW (source_size_expr) : 0;

  // size check for concrete types
  // TODO(liushuyu): check alignment for pointers; check for dependently-sized
  // types
  if (target_size != source_size)
    {
      rust_error_at (fntype->get_locus (),
		     "cannot transmute between types of different sizes, or "
		     "dependently-sized types");
      rust_inform (
	fntype->get_ident ().locus, "source type: %qs (%lu bits)",
	fntype->get_params ().at (0).get_type ()->as_string ().c_str (),
	(unsigned long) source_size);
      rust_inform (fntype->get_ident ().locus, "target type: %qs (%lu bits)",
		   fntype->get_return_type ()->as_string ().c_str (),
		   (unsigned long) target_size);
    }

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN transmute FN BODY BEGIN

  // Return *((orig_type*)&decl)  */

  tree t = build_fold_addr_expr_loc (UNKNOWN_LOCATION, convert_me_expr);
  t = fold_build1_loc (UNKNOWN_LOCATION, NOP_EXPR,
		       build_pointer_type (target_type_expr), t);
  tree result_expr = build_fold_indirect_ref_loc (UNKNOWN_LOCATION, t);

  auto return_statement
    = Backend::return_statement (fndecl, result_expr, UNDEF_LOCATION);
  ctx->add_statement (return_statement);
  // BUILTIN transmute FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
sizeof_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  // size_of has _zero_ parameters its parameter is the generic one
  rust_assert (fntype->get_params ().size () == 0);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // get the template parameter type tree fn size_of<T>();
  rust_assert (fntype->get_num_substitutions () == 1);
  auto &param_mapping = fntype->get_substs ().at (0);
  const auto param_tyty = param_mapping.get_param_ty ();
  auto resolved_tyty = param_tyty->resolve ();
  tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, resolved_tyty);

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN size_of FN BODY BEGIN
  tree size_expr = TYPE_SIZE_UNIT (template_parameter_type);
  auto return_statement
    = Backend::return_statement (fndecl, size_expr, UNDEF_LOCATION);
  ctx->add_statement (return_statement);
  // BUILTIN size_of FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

/**
 * pub fn size_of_val<T: ?Sized>(_: *const T) -> usize;
 */
tree
size_of_val_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  rust_assert (fntype->get_params ().size () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  auto locus = fntype->get_locus ();

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto &__param = param_vars.at (0);
  rust_assert (param_vars.size () == 1);
  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  rust_assert (fntype->get_num_substitutions () == 1);
  auto &param_mapping = fntype->get_substs ().at (0);
  const auto param_tyty = param_mapping.get_param_ty ();
  auto resolved_tyty = param_tyty->resolve ();
  tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, resolved_tyty);

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN size_of FN BODY BEGIN

  tree size_expr = NULL_TREE;
  if (RS_DST_FLAG_P (template_parameter_type))
    {
      tree param = Backend::var_expression (__param, UNDEF_LOCATION);
      tree param_ty = TREE_TYPE (param);
      tree data_field = TYPE_FIELDS (param_ty);
      tree meta_field = DECL_CHAIN (data_field);
      tree meta_field_expr
	= build3_loc (locus, COMPONENT_REF, TREE_TYPE (meta_field), param,
		      meta_field, NULL_TREE);

      if (resolved_tyty->get_kind () == TyTy::TypeKind::SLICE
	  || resolved_tyty->get_kind () == TyTy::TypeKind::STR)
	{
	  tree elem_type = NULL_TREE;
	  if (resolved_tyty->get_kind () == TyTy::TypeKind::SLICE)
	    {
	      auto slice_tyty = static_cast<TyTy::SliceType *> (resolved_tyty);
	      elem_type
		= TyTyResolveCompile::compile (ctx,
					       slice_tyty->get_element_type ());
	    }
	  else
	    elem_type = char_type_node;

	  tree elem_size = TYPE_SIZE_UNIT (elem_type);
	  size_expr
	    = build2_loc (locus, MULT_EXPR, size_type_node,
			  fold_convert_loc (locus, size_type_node,
					    meta_field_expr),
			  fold_convert_loc (locus, size_type_node, elem_size));
	}
      else if (resolved_tyty->get_kind () == TyTy::TypeKind::DYNAMIC)
	{
	  tree vtable_ptr_ty = TREE_TYPE (meta_field_expr);
	  tree vtable_ty = TREE_TYPE (vtable_ptr_ty);

	  tree vtable_ref
	    = build1_loc (locus, INDIRECT_REF, vtable_ty, meta_field_expr);

	  tree vtable_field_0 = TYPE_FIELDS (vtable_ty);
	  tree vtable_field_size = DECL_CHAIN (vtable_field_0);
	  rust_assert (vtable_field_size != NULL_TREE);

	  size_expr
	    = build3_loc (locus, COMPONENT_REF, TREE_TYPE (vtable_field_size),
			  vtable_ref, vtable_field_size, NULL_TREE);
	}
      else
	{
	  rust_unreachable ();
	}
    }
  else
    size_expr = TYPE_SIZE_UNIT (template_parameter_type);

  auto return_statement
    = Backend::return_statement (fndecl, size_expr, UNDEF_LOCATION);
  ctx->add_statement (return_statement);

  // BUILTIN size_of FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

/**
 * pub fn min_align_of<T>() -> usize;
 */
tree
min_align_of_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  // min_align_of has _zero_ parameters its parameter is the generic one
  rust_assert (fntype->get_params ().size () == 0);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // get the template parameter type tree fn min_align_of<T>();
  rust_assert (fntype->get_num_substitutions () == 1);
  auto &param_mapping = fntype->get_substs ().at (0);
  const auto param_tyty = param_mapping.get_param_ty ();
  auto resolved_tyty = param_tyty->resolve ();
  tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, resolved_tyty);

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN min_align_of FN BODY BEGIN
  tree align_expr
    = build_int_cst (size_type_node, TYPE_ALIGN_UNIT (template_parameter_type));

  auto return_statement
    = Backend::return_statement (fndecl, align_expr, UNDEF_LOCATION);
  ctx->add_statement (return_statement);
  // BUILTIN min_align_of FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

/**
 * pub fn min_align_of_val<T: ?Sized>(_: *const T) -> usize;
 */
tree
min_align_of_val_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  rust_assert (fntype->get_params ().size () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  auto locus = fntype->get_locus ();

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto &__param = param_vars.at (0);
  rust_assert (param_vars.size () == 1);
  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  rust_assert (fntype->get_num_substitutions () == 1);
  auto &param_mapping = fntype->get_substs ().at (0);
  const auto param_tyty = param_mapping.get_param_ty ();
  auto resolved_tyty = param_tyty->resolve ();
  tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, resolved_tyty);

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN size_of FN BODY BEGIN

  tree align_expr = NULL_TREE;
  if (RS_DST_FLAG_P (template_parameter_type))
    {
      tree param = Backend::var_expression (__param, UNDEF_LOCATION);
      tree param_ty = TREE_TYPE (param);
      tree data_field = TYPE_FIELDS (param_ty);
      tree meta_field = DECL_CHAIN (data_field);
      tree meta_field_expr
	= build3_loc (locus, COMPONENT_REF, TREE_TYPE (meta_field), param,
		      meta_field, NULL_TREE);

      if (resolved_tyty->get_kind () == TyTy::TypeKind::SLICE
	  || resolved_tyty->get_kind () == TyTy::TypeKind::STR)
	{
	  tree elem_type = NULL_TREE;
	  if (resolved_tyty->get_kind () == TyTy::TypeKind::SLICE)
	    {
	      auto slice_tyty = static_cast<TyTy::SliceType *> (resolved_tyty);
	      elem_type
		= TyTyResolveCompile::compile (ctx,
					       slice_tyty->get_element_type ());
	    }
	  else
	    elem_type = char_type_node;

	  align_expr
	    = build_int_cst (size_type_node, TYPE_ALIGN_UNIT (elem_type));
	}
      else if (resolved_tyty->get_kind () == TyTy::TypeKind::DYNAMIC)
	{
	  tree vtable_ptr_ty = TREE_TYPE (meta_field_expr);
	  tree vtable_ty = TREE_TYPE (vtable_ptr_ty);

	  tree vtable_ref
	    = build1_loc (locus, INDIRECT_REF, vtable_ty, meta_field_expr);

	  tree vtable_field_0 = TYPE_FIELDS (vtable_ty);
	  tree vtable_field_1 = DECL_CHAIN (vtable_field_0);
	  tree vtable_field_align = DECL_CHAIN (vtable_field_1);
	  rust_assert (vtable_field_align != NULL_TREE);

	  align_expr
	    = build3_loc (locus, COMPONENT_REF, TREE_TYPE (vtable_field_align),
			  vtable_ref, vtable_field_align, NULL_TREE);
	}
      else
	{
	  rust_unreachable ();
	}
    }
  else
    align_expr = build_int_cst (size_type_node,
				TYPE_ALIGN_UNIT (template_parameter_type));

  auto return_statement
    = Backend::return_statement (fndecl, align_expr, UNDEF_LOCATION);
  ctx->add_statement (return_statement);

  // BUILTIN size_of FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
offset (Context *ctx, TyTy::FnType *fntype, location_t expr_locus)
{
  // offset intrinsic has two params dst pointer and offset isize
  rust_assert (fntype->get_params ().size () == 2);

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto &dst_param = param_vars.at (0);
  auto &size_param = param_vars.at (1);
  rust_assert (param_vars.size () == 2);
  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN offset FN BODY BEGIN
  tree dst = Backend::var_expression (dst_param, UNDEF_LOCATION);
  tree size = Backend::var_expression (size_param, UNDEF_LOCATION);
  tree pointer_offset_expr = pointer_offset_expression (dst, size, expr_locus);
  auto return_statement
    = Backend::return_statement (fndecl, pointer_offset_expr, UNDEF_LOCATION);
  ctx->add_statement (return_statement);
  // BUILTIN offset FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

/**
 * pub const fn bswap<T: Copy>(x: T) -> T;
 */
tree
bswap_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  rust_assert (fntype->get_params ().size () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  auto locus = fntype->get_locus ();

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto &x_param = param_vars.at (0);
  rust_assert (param_vars.size () == 1);
  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  auto *monomorphized_type
    = fntype->get_substs ().at (0).get_param_ty ()->resolve ();

  auto call_locus = ctx->get_mappings ().lookup_location (fntype->get_ref ());
  check_for_basic_integer_type ("bswap", call_locus, monomorphized_type);

  tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, monomorphized_type);

  tree size_expr = TYPE_SIZE_UNIT (template_parameter_type);
  unsigned HOST_WIDE_INT size = TREE_INT_CST_LOW (size_expr);

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN bswap FN BODY BEGIN

  auto expr_x = Backend::var_expression (x_param, locus);
  tree result = NULL_TREE;

  if (size == 1)
    {
      result = expr_x;
    }
  else
    {
      tree target_type = NULL_TREE;
      const char *builtin_name = nullptr;
      switch (size)
	{
	case 2:
	  builtin_name = "__builtin_bswap16";
	  target_type = uint16_type_node;
	  break;
	case 4:
	  builtin_name = "__builtin_bswap32";
	  target_type = uint32_type_node;
	  break;
	case 8:
	  builtin_name = "__builtin_bswap64";
	  target_type = uint64_type_node;
	  break;
	case 16:
	  builtin_name = "__builtin_bswap128";
	  target_type = uint128_type_node;
	  break;
	default:
	  return error_mark_node;
	}

      tree bswap_raw = nullptr;
      auto ok = BuiltinsContext::get ().lookup_simple_builtin (builtin_name,
							       &bswap_raw);

      if (ok)
	{
	  tree bswap_fn = build_fold_addr_expr_loc (locus, bswap_raw);

	  auto bswap_x = build1 (CONVERT_EXPR, target_type, expr_x);

	  auto bswap_call
	    = Backend::call_expression (bswap_fn, {bswap_x}, NULL_TREE, locus);

	  result = build1 (CONVERT_EXPR, template_parameter_type, bswap_call);
	}
      else
	{
	  auto ok2 = BuiltinsContext::get ().lookup_simple_builtin (
	    "__builtin_bswap64", &bswap_raw);
	  rust_assert (ok2);

	  tree bswap_fn = build_fold_addr_expr_loc (locus, bswap_raw);

	  tree tmp_in_stmt = error_mark_node;
	  Bvariable *in_var
	    = Backend::temporary_variable (fndecl, NULL_TREE,
					   template_parameter_type, expr_x,
					   true, locus, &tmp_in_stmt);
	  ctx->add_statement (tmp_in_stmt);

	  tree addr_x
	    = build_fold_addr_expr_loc (locus, in_var->get_tree (locus));
	  tree u64_ptr_type = build_pointer_type (uint64_type_node);

	  tree low_ptr = fold_convert (u64_ptr_type, addr_x);
	  tree low = build_fold_indirect_ref_loc (locus, low_ptr);

	  tree high_ptr = fold_build2 (POINTER_PLUS_EXPR, u64_ptr_type, low_ptr,
				       size_int (8));
	  tree high = build_fold_indirect_ref_loc (locus, high_ptr);

	  auto new_high
	    = Backend::call_expression (bswap_fn, {low}, NULL_TREE, locus);
	  auto new_low
	    = Backend::call_expression (bswap_fn, {high}, NULL_TREE, locus);

	  tree tmp_stmt = error_mark_node;
	  Bvariable *result_var
	    = Backend::temporary_variable (fndecl, NULL_TREE,
					   template_parameter_type, NULL_TREE,
					   true, locus, &tmp_stmt);
	  ctx->add_statement (tmp_stmt);

	  tree addr_res
	    = build_fold_addr_expr_loc (locus, result_var->get_tree (locus));
	  tree res_ptr = fold_convert (u64_ptr_type, addr_res);

	  tree store_low
	    = build2 (MODIFY_EXPR, void_type_node,
		      build_fold_indirect_ref_loc (locus, res_ptr), new_low);
	  ctx->add_statement (store_low);

	  tree res_high_ptr = fold_build2 (POINTER_PLUS_EXPR, u64_ptr_type,
					   res_ptr, size_int (8));
	  tree store_high
	    = build2 (MODIFY_EXPR, void_type_node,
		      build_fold_indirect_ref_loc (locus, res_high_ptr),
		      new_high);
	  ctx->add_statement (store_high);

	  result = result_var->get_tree (locus);
	}
    }

  auto return_statement = Backend::return_statement (fndecl, result, locus);

  TREE_READONLY (result) = 1;

  ctx->add_statement (return_statement);

  // BUILTIN bswap FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

tree
ctlz_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  return inner::ctlz_handler (ctx, fntype, false);
}

tree
ctlz_nonzero_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  return inner::ctlz_handler (ctx, fntype, true);
}

tree
cttz_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  return inner::cttz_handler (ctx, fntype, false);
}

tree
cttz_nonzero_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  return inner::cttz_handler (ctx, fntype, true);
}

/**
 * pub unsafe fn write_bytes<T>(dst: *mut T, val: u8, count: usize);
 */
tree
write_bytes_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  rust_assert (fntype->get_params ().size () == 3);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  tree fndecl = compile_intrinsic_function (ctx, fntype);

  auto locus = fntype->get_locus ();

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto &dst_param = param_vars.at (0);
  auto &val_param = param_vars.at (1);
  auto &count_param = param_vars.at (2);
  rust_assert (param_vars.size () == 3);
  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  auto *monomorphized_type
    = fntype->get_substs ().at (0).get_param_ty ()->resolve ();

  tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, monomorphized_type);
  tree dst_size_expr = TYPE_SIZE_UNIT (template_parameter_type);
  rust_assert (dst_size_expr != NULL_TREE);

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN WRITE_BYTES FN BODY START

  tree expr_dst = Backend::var_expression (dst_param, locus);
  tree expr_val = Backend::var_expression (val_param, locus);
  tree expr_count = Backend::var_expression (count_param, locus);

  tree expr_count_final
    = fold_build2_loc (locus, MULT_EXPR, size_type_node,
		       fold_convert_loc (locus, size_type_node, expr_count),
		       fold_convert_loc (locus, size_type_node, dst_size_expr));

  tree write_bytes_raw = nullptr;
  // void* memset( void* dest, int ch, size_t count );
  bool ok = BuiltinsContext::get ().lookup_simple_builtin ("__builtin_memset",
							   &write_bytes_raw);
  rust_assert (ok);

  tree write_bytes_fn = build_fold_addr_expr_loc (locus, write_bytes_raw);
  tree write_bytes_call
    = Backend::call_expression (write_bytes_fn,
				{expr_dst, expr_val, expr_count_final},
				NULL_TREE, locus);

  ctx->add_statement (write_bytes_call);

  // BUILTIN WRITE_BYTES FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  TREE_READONLY (fndecl) = 0;

  return fndecl;
}

/**
 * pub fn arith_offset<T>(dst: *const T, offset: isize) -> *const T;
 */
tree
arith_offset_handler (Context *ctx, TyTy::FnType *fntype, location_t expr_locus)
{
  rust_assert (fntype->get_params ().size () == 2);

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  auto locus = fntype->get_locus ();

  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  auto &dst_param = param_vars.at (0);
  auto &size_param = param_vars.at (1);
  rust_assert (param_vars.size () == 2);
  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN arith_offset FN BODY BEGIN

  tree dst = Backend::var_expression (dst_param, locus);
  tree size = Backend::var_expression (size_param, locus);
  tree pointer_offset_expr = pointer_offset_expression (dst, size, expr_locus);
  auto return_statement
    = Backend::return_statement (fndecl, pointer_offset_expr, locus);
  ctx->add_statement (return_statement);

  // BUILTIN arith_offset FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

/**
 * pub fn assert_zero_valid<T>();
 *
 * TODO: Since gccrs currently lacks comprehensive layout engine support for
 * validity ranges (such as `rustc_layout_scalar_valid_range_start`), we cannot
 * accurately determine if a type is safely zeroable. Therefore, this is
 * implemented as a temporary no-op stub that always returns void (unit) and
 * succeeds for all types.
 */
tree
assert_zero_valid_handler (Context *ctx, TyTy::FnType *fntype, location_t)
{
  rust_assert (fntype->get_params ().size () == 0);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  rust_assert (fntype->get_num_substitutions () == 1);
  auto &param_mapping = fntype->get_substs ().at (0);
  const auto param_tyty = param_mapping.get_param_ty ();
  auto resolved_tyty = param_tyty->resolve ();

  // Safely resolve the template parameter type to ensure monomorphization
  // works.
  // Suppress unused-variable warning since layout verification is a FIXME.
  [[gnu::unused]] tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, resolved_tyty);

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN assert_zero_valid FN BODY BEGIN

  // TODO: Implement layout verification via template_parameter_type once
  // niche-filling and valid scalar ranges are supported in the layout engine.
  // If invalid, this should emit a panic.

  // we always return unit for now.
  auto return_statement
    = Backend::return_statement (fndecl, void_node, UNDEF_LOCATION);
  ctx->add_statement (return_statement);

  // BUILTIN assert_zero_valid FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

} // namespace handlers
} // namespace Compile
} // namespace Rust
