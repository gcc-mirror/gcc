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
#include "rust-compile-type.h"
#include "rust-compile-expr.h"
#include "rust-compile-fnparam.h"
#include "rust-builtins.h"
#include "rust-diagnostics.h"
#include "rust-location.h"
#include "rust-constexpr.h"
#include "rust-tree.h"
#include "tree-core.h"
#include "rust-gcc.h"
#include "print-tree.h"
#include "fold-const.h"
#include "langhooks.h"
#include "rust-gcc.h"
#include "rust-constexpr.h"

#include "print-tree.h"

// declaration taken from "stringpool.h"
// the get_identifier macro causes compilation issues
extern tree
get_identifier (const char *);

namespace Rust {
namespace Compile {

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

static bool
check_for_basic_integer_type (const std::string &intrinsic_str,
			      location_t locus, TyTy::BaseType *type)
{
  auto is_basic_integer = is_basic_integer_type (type);
  if (!is_basic_integer)
    {
      rust_error_at (
	locus,
	"%s intrinsics can only be used with basic integer types (got %qs)",
	intrinsic_str.c_str (), type->get_name ().c_str ());
    }

  return is_basic_integer;
}

static tree
offset_handler (Context *ctx, TyTy::FnType *fntype);
static tree
sizeof_handler (Context *ctx, TyTy::FnType *fntype);
static tree
transmute_handler (Context *ctx, TyTy::FnType *fntype);
static tree
rotate_handler (Context *ctx, TyTy::FnType *fntype, tree_code op);
static tree
wrapping_op_handler_inner (Context *ctx, TyTy::FnType *fntype, tree_code op);
static tree
op_with_overflow_inner (Context *ctx, TyTy::FnType *fntype, tree_code op);
static tree
uninit_handler (Context *ctx, TyTy::FnType *fntype);
static tree
move_val_init_handler (Context *ctx, TyTy::FnType *fntype);
static tree
assume_handler (Context *ctx, TyTy::FnType *fntype);

enum class Prefetch
{
  Read,
  Write
};

static tree
prefetch_data_handler (Context *ctx, TyTy::FnType *fntype, Prefetch kind);

static inline tree
rotate_left_handler (Context *ctx, TyTy::FnType *fntype)
{
  return rotate_handler (ctx, fntype, LROTATE_EXPR);
}
static inline tree
rotate_right_handler (Context *ctx, TyTy::FnType *fntype)
{
  return rotate_handler (ctx, fntype, RROTATE_EXPR);
}

const static std::function<tree (Context *, TyTy::FnType *)>
wrapping_op_handler (tree_code op)
{
  return [op] (Context *ctx, TyTy::FnType *fntype) {
    return wrapping_op_handler_inner (ctx, fntype, op);
  };
}

const static std::function<tree (Context *, TyTy::FnType *)>
op_with_overflow (tree_code op)
{
  return [op] (Context *ctx, TyTy::FnType *fntype) {
    return op_with_overflow_inner (ctx, fntype, op);
  };
}

static inline tree
prefetch_read_data (Context *ctx, TyTy::FnType *fntype)
{
  return prefetch_data_handler (ctx, fntype, Prefetch::Read);
}
static inline tree
prefetch_write_data (Context *ctx, TyTy::FnType *fntype)
{
  return prefetch_data_handler (ctx, fntype, Prefetch::Write);
}

static tree
atomic_store_handler_inner (Context *ctx, TyTy::FnType *fntype, int ordering);
static tree
atomic_load_handler_inner (Context *ctx, TyTy::FnType *fntype, int ordering);

static inline std::function<tree (Context *, TyTy::FnType *)>
atomic_store_handler (int ordering)
{
  return [ordering] (Context *ctx, TyTy::FnType *fntype) {
    return atomic_store_handler_inner (ctx, fntype, ordering);
  };
}

static inline std::function<tree (Context *, TyTy::FnType *)>
atomic_load_handler (int ordering)
{
  return [ordering] (Context *ctx, TyTy::FnType *fntype) {
    return atomic_load_handler_inner (ctx, fntype, ordering);
  };
}

static inline tree
unchecked_op_inner (Context *ctx, TyTy::FnType *fntype, tree_code op);

const static std::function<tree (Context *, TyTy::FnType *)>
unchecked_op_handler (tree_code op)
{
  return [op] (Context *ctx, TyTy::FnType *fntype) {
    return unchecked_op_inner (ctx, fntype, op);
  };
}

static inline tree
copy_handler_inner (Context *ctx, TyTy::FnType *fntype, bool overlaps);

const static std::function<tree (Context *, TyTy::FnType *)>
copy_handler (bool overlaps)
{
  return [overlaps] (Context *ctx, TyTy::FnType *fntype) {
    return copy_handler_inner (ctx, fntype, overlaps);
  };
}

static inline tree
expect_handler_inner (Context *ctx, TyTy::FnType *fntype, bool likely);

const static std::function<tree (Context *, TyTy::FnType *)>
expect_handler (bool likely)
{
  return [likely] (Context *ctx, TyTy::FnType *fntype) {
    return expect_handler_inner (ctx, fntype, likely);
  };
}

inline tree
sorry_handler (Context *ctx, TyTy::FnType *fntype)
{
  rust_sorry_at (fntype->get_locus (), "intrinsic %qs is not yet implemented",
		 fntype->get_identifier ().c_str ());

  return error_mark_node;
}

static const std::map<std::string,
		      std::function<tree (Context *, TyTy::FnType *)>>
  generic_intrinsics = {
    {"offset", offset_handler},
    {"size_of", sizeof_handler},
    {"transmute", transmute_handler},
    {"rotate_left", rotate_left_handler},
    {"rotate_right", rotate_right_handler},
    {"wrapping_add", wrapping_op_handler (PLUS_EXPR)},
    {"wrapping_sub", wrapping_op_handler (MINUS_EXPR)},
    {"wrapping_mul", wrapping_op_handler (MULT_EXPR)},
    {"add_with_overflow", op_with_overflow (PLUS_EXPR)},
    {"sub_with_overflow", op_with_overflow (MINUS_EXPR)},
    {"mul_with_overflow", op_with_overflow (MULT_EXPR)},
    {"copy", copy_handler (true)},
    {"copy_nonoverlapping", copy_handler (false)},
    {"prefetch_read_data", prefetch_read_data},
    {"prefetch_write_data", prefetch_write_data},
    {"atomic_store_seqcst", atomic_store_handler (__ATOMIC_SEQ_CST)},
    {"atomic_store_release", atomic_store_handler (__ATOMIC_RELEASE)},
    {"atomic_store_relaxed", atomic_store_handler (__ATOMIC_RELAXED)},
    {"atomic_store_unordered", atomic_store_handler (__ATOMIC_RELAXED)},
    {"atomic_load_seqcst", atomic_load_handler (__ATOMIC_SEQ_CST)},
    {"atomic_load_acquire", atomic_load_handler (__ATOMIC_ACQUIRE)},
    {"atomic_load_relaxed", atomic_load_handler (__ATOMIC_RELAXED)},
    {"atomic_load_unordered", atomic_load_handler (__ATOMIC_RELAXED)},
    {"unchecked_add", unchecked_op_handler (PLUS_EXPR)},
    {"unchecked_sub", unchecked_op_handler (MINUS_EXPR)},
    {"unchecked_mul", unchecked_op_handler (MULT_EXPR)},
    {"unchecked_div", unchecked_op_handler (TRUNC_DIV_EXPR)},
    {"unchecked_rem", unchecked_op_handler (TRUNC_MOD_EXPR)},
    {"unchecked_shl", unchecked_op_handler (LSHIFT_EXPR)},
    {"unchecked_shr", unchecked_op_handler (RSHIFT_EXPR)},
    {"uninit", uninit_handler},
    {"move_val_init", move_val_init_handler},
    {"likely", expect_handler (true)},
    {"unlikely", expect_handler (false)},
    {"assume", assume_handler},
};

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
Intrinsics::compile (TyTy::FnType *fntype)
{
  rust_assert (fntype->get_abi () == ABI::INTRINSIC);

  tree builtin = error_mark_node;
  BuiltinsContext &builtin_ctx = BuiltinsContext::get ();

  if (builtin_ctx.lookup_simple_builtin (fntype->get_identifier (), &builtin))
    return builtin;

  // is it an generic builtin?
  auto it = generic_intrinsics.find (fntype->get_identifier ());
  if (it != generic_intrinsics.end ())
    return it->second (ctx, fntype);

  location_t locus = ctx->get_mappings ()->lookup_location (fntype->get_ref ());
  rust_error_at (locus, ErrorCode::E0093,
		 "unrecognized intrinsic function: %<%s%>",
		 fntype->get_identifier ().c_str ());

  return error_mark_node;
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

/**
 * Maybe override the Hir Lookups for the substituions in this context
 */
static void
maybe_override_ctx (TyTy::FnType *fntype)
{
  if (fntype->has_substitutions_defined ())
    fntype->override_context ();
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
      auto &referenced_param = parm.first;
      auto &param_tyty = parm.second;
      auto compiled_param_type = TyTyResolveCompile::compile (ctx, param_tyty);

      location_t param_locus = referenced_param->get_locus ();
      Bvariable *compiled_param_var
	= CompileFnParam::compile (ctx, fndecl, referenced_param,
				   compiled_param_type, param_locus);

      compiled_param_variables->push_back (compiled_param_var);
      if (compiled_param_types)
	compiled_param_types->push_back (compiled_param_type);
    }
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

static tree
offset_handler (Context *ctx, TyTy::FnType *fntype)
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
  tree pointer_offset_expr
    = pointer_offset_expression (dst, size, BUILTINS_LOCATION);
  auto return_statement
    = Backend::return_statement (fndecl, pointer_offset_expr, UNDEF_LOCATION);
  ctx->add_statement (return_statement);
  // BUILTIN offset FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

static tree
sizeof_handler (Context *ctx, TyTy::FnType *fntype)
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
  const TyTy::ParamType *param_tyty = param_mapping.get_param_ty ();
  TyTy::BaseType *resolved_tyty = param_tyty->resolve ();
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

static tree
transmute_handler (Context *ctx, TyTy::FnType *fntype)
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
      rust_inform (fntype->get_ident ().locus, "source type: %qs (%lu bits)",
		   fntype->get_params ().at (0).second->as_string ().c_str (),
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

static tree
rotate_handler (Context *ctx, TyTy::FnType *fntype, tree_code op)
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

/**
 * pub fn wrapping_{add, sub, mul}<T>(lhs: T, rhs: T) -> T;
 */
static tree
wrapping_op_handler_inner (Context *ctx, TyTy::FnType *fntype, tree_code op)
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
static tree
op_with_overflow_inner (Context *ctx, TyTy::FnType *fntype, tree_code op)
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
  const TyTy::ParamType *param_tyty = param_mapping.get_param_ty ();
  TyTy::BaseType *resolved_tyty = param_tyty->resolve ();
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
static tree
copy_handler_inner (Context *ctx, TyTy::FnType *fntype, bool overlaps)
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

static tree
make_unsigned_long_tree (unsigned long value)
{
  return build_int_cst (integer_type_node, value);
}

static tree
prefetch_data_handler (Context *ctx, TyTy::FnType *fntype, Prefetch kind)
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

  if (!check_for_basic_integer_type ("atomic", locus, operand_type))
    return "";

  result += type_size_str->second;

  return result;
}

static tree
atomic_store_handler_inner (Context *ctx, TyTy::FnType *fntype, int ordering)
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

  auto builtin_name
    = build_atomic_builtin_name ("atomic_store_", fntype->get_locus (),
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

static tree
atomic_load_handler_inner (Context *ctx, TyTy::FnType *fntype, int ordering)
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

static inline tree
unchecked_op_inner (Context *ctx, TyTy::FnType *fntype, tree_code op)
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

  check_for_basic_integer_type ("unchecked operation", fntype->get_locus (),
				monomorphized_type);

  auto expr = build2 (op, TREE_TYPE (x), x, y);
  auto return_statement
    = Backend::return_statement (fndecl, expr, UNDEF_LOCATION);

  ctx->add_statement (return_statement);

  // BUILTIN unchecked_<op> BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

static tree
uninit_handler (Context *ctx, TyTy::FnType *fntype)
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
  const TyTy::ParamType *param_tyty = param_mapping.get_param_ty ();
  TyTy::BaseType *resolved_tyty = param_tyty->resolve ();
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

static tree
move_val_init_handler (Context *ctx, TyTy::FnType *fntype)
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
  const TyTy::ParamType *param_tyty = param_mapping.get_param_ty ();
  TyTy::BaseType *resolved_tyty = param_tyty->resolve ();
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

static inline tree
expect_handler_inner (Context *ctx, TyTy::FnType *fntype, bool likely)
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

static tree
assume_handler (Context *ctx, TyTy::FnType *fntype)
{
  // TODO: make sure this is actually helping the compiler optimize

  rust_assert (fntype->get_params ().size () == 1);
  rust_assert (fntype->param_at (0).second->get_kind ()
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
  // BUILTIN size_of FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

} // namespace Compile
} // namespace Rust
