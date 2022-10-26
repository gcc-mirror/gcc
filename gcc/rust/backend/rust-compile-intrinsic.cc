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
#include "print-tree.h"
#include "fold-const.h"
#include "langhooks.h"

#include "print-tree.h"

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
check_for_basic_integer_type (const std::string &intrinsic_str, Location locus,
			      TyTy::BaseType *type)
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
copy_nonoverlapping_handler (Context *ctx, TyTy::FnType *fntype);

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
    {"copy_nonoverlapping", copy_nonoverlapping_handler},
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
};

Intrinsics::Intrinsics (Context *ctx) : ctx (ctx) {}

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

  Location locus = ctx->get_mappings ()->lookup_location (fntype->get_ref ());
  rust_error_at (locus, "unknown builtin intrinsic: %s",
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
  if (ctx->lookup_function_decl (fntype->get_ty_ref (), lookup,
				 fntype->get_id (), fntype))
    {
      // Has this been added to the list? Then it must be finished
      if (ctx->function_completed (*lookup))
	{
	  tree dummy = NULL_TREE;
	  if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &dummy))
	    ctx->insert_function_decl (fntype, *lookup);
	  return true;
	}
    }

  return false;
}

/**
 * Maybe override the Hir Lookups for the substituions in this context
 */
static void
maybe_override_ctx (TyTy::FnType *fntype)
{
  if (fntype->has_subsititions_defined ())
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

      Location param_locus = referenced_param->get_locus ();
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
  tree fndecl
    = ctx->get_backend ()->function (compiled_fn_type, ir_symbol_name, asm_name,
				     flags, fntype->get_ident ().locus);

  TREE_PUBLIC (fndecl) = 0;
  TREE_READONLY (fndecl) = 1;
  DECL_ARTIFICIAL (fndecl) = 1;
  DECL_EXTERNAL (fndecl) = 0;
  DECL_DECLARED_INLINE_P (fndecl) = 1;

  return fndecl;
}

static void
enter_intrinsic_block (Context *ctx, tree fndecl)
{
  tree enclosing_scope = NULL_TREE;
  Location start_location = Location ();
  Location end_location = Location ();

  auto block = ctx->get_backend ()->block (fndecl, enclosing_scope, {},
					   start_location, end_location);

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
  if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN offset FN BODY BEGIN
  tree dst = ctx->get_backend ()->var_expression (dst_param, Location ());
  tree size = ctx->get_backend ()->var_expression (size_param, Location ());
  tree pointer_offset_expr
    = pointer_offset_expression (dst, size, BUILTINS_LOCATION);
  auto return_statement
    = ctx->get_backend ()->return_statement (fndecl, {pointer_offset_expr},
					     Location ());
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
    = ctx->get_backend ()->return_statement (fndecl, {size_expr}, Location ());
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

  if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  // param to convert
  Bvariable *convert_me_param = param_vars.at (0);
  tree convert_me_expr
    = ctx->get_backend ()->var_expression (convert_me_param, Location ());

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

  tree t
    = build_fold_addr_expr_loc (Location ().gcc_location (), convert_me_expr);
  t = fold_build1_loc (Location ().gcc_location (), NOP_EXPR,
		       build_pointer_type (target_type_expr), t);
  tree result_expr
    = build_fold_indirect_ref_loc (Location ().gcc_location (), t);

  auto return_statement
    = ctx->get_backend ()->return_statement (fndecl, {result_expr},
					     Location ());
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
  if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN rotate FN BODY BEGIN
  tree x = ctx->get_backend ()->var_expression (x_param, Location ());
  tree y = ctx->get_backend ()->var_expression (y_param, Location ());
  tree rotate_expr
    = fold_build2_loc (BUILTINS_LOCATION, op, TREE_TYPE (x), x, y);
  auto return_statement
    = ctx->get_backend ()->return_statement (fndecl, {rotate_expr},
					     Location ());
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

  if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN wrapping_<op> FN BODY BEGIN
  auto lhs = ctx->get_backend ()->var_expression (lhs_param, Location ());
  auto rhs = ctx->get_backend ()->var_expression (rhs_param, Location ());

  // Operations are always wrapping in Rust, as we have -fwrapv enabled by
  // default. The difference between a wrapping_{add, sub, mul} and a regular
  // arithmetic operation is that these intrinsics do not panic - they always
  // carry over.
  auto wrap_expr = build2 (op, TREE_TYPE (lhs), lhs, rhs);

  auto return_statement
    = ctx->get_backend ()->return_statement (fndecl, {wrap_expr}, Location ());
  ctx->add_statement (return_statement);
  // BUILTIN wrapping_<op> FN BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

/**
 * fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
 */
static tree
copy_nonoverlapping_handler (Context *ctx, TyTy::FnType *fntype)
{
  rust_assert (fntype->get_params ().size () == 3);
  rust_assert (fntype->get_num_substitutions () == 1);

  tree lookup = NULL_TREE;
  if (check_for_cached_intrinsic (ctx, fntype, &lookup))
    return lookup;

  auto fndecl = compile_intrinsic_function (ctx, fntype);

  // Most intrinsic functions are pure - not `copy_nonoverlapping`
  TREE_READONLY (fndecl) = 0;
  TREE_SIDE_EFFECTS (fndecl) = 1;

  // setup the params
  std::vector<Bvariable *> param_vars;
  compile_fn_params (ctx, fntype, fndecl, &param_vars);

  if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN copy_nonoverlapping BODY BEGIN

  auto src = ctx->get_backend ()->var_expression (param_vars[0], Location ());
  auto dst = ctx->get_backend ()->var_expression (param_vars[1], Location ());
  auto count = ctx->get_backend ()->var_expression (param_vars[2], Location ());

  // We want to create the following statement
  // memcpy(dst, src, size_of::<T>());
  // so
  // memcpy(dst, src, size_expr);

  auto *resolved_ty = fntype->get_substs ().at (0).get_param_ty ()->resolve ();
  auto param_type = TyTyResolveCompile::compile (ctx, resolved_ty);

  tree size_expr
    = build2 (MULT_EXPR, size_type_node, TYPE_SIZE_UNIT (param_type), count);

  tree memcpy_raw = nullptr;
  BuiltinsContext::get ().lookup_simple_builtin ("memcpy", &memcpy_raw);
  rust_assert (memcpy_raw);
  auto memcpy
    = build_fold_addr_expr_loc (Location ().gcc_location (), memcpy_raw);

  auto copy_call
    = ctx->get_backend ()->call_expression (memcpy, {dst, src, size_expr},
					    nullptr, Location ());

  ctx->add_statement (copy_call);

  // BUILTIN copy_nonoverlapping BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

static tree
make_unsigned_long_tree (Context *ctx, unsigned long value)
{
  mpz_t mpz_value;
  mpz_init_set_ui (mpz_value, value);

  return ctx->get_backend ()->integer_constant_expression (integer_type_node,
							   mpz_value);
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

  if (!ctx->get_backend ()->function_set_parameters (fndecl, args))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  auto addr = ctx->get_backend ()->var_expression (args[0], Location ());
  auto locality = ctx->get_backend ()->var_expression (args[1], Location ());
  auto rw_flag = make_unsigned_long_tree (ctx, kind == Prefetch::Write ? 1 : 0);

  auto prefetch_raw = NULL_TREE;
  auto ok
    = BuiltinsContext::get ().lookup_simple_builtin ("prefetch", &prefetch_raw);
  rust_assert (ok);
  auto prefetch
    = build_fold_addr_expr_loc (Location ().gcc_location (), prefetch_raw);

  auto prefetch_call
    = ctx->get_backend ()->call_expression (prefetch, {addr, rw_flag, locality},
					    nullptr, Location ());

  TREE_READONLY (prefetch_call) = 0;
  TREE_SIDE_EFFECTS (prefetch_call) = 1;

  ctx->add_statement (prefetch_call);

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

static std::string
build_atomic_builtin_name (const std::string &prefix, Location locus,
			   TyTy::BaseType *operand_type)
{
  static const std::map<std::string, std::string> allowed_types = {
    {"i8", "1"},    {"i16", "2"},   {"i32", "4"},   {"i64", "8"},
    {"i128", "16"}, {"isize", "8"}, {"u8", "1"},    {"u16", "2"},
    {"u32", "4"},   {"u64", "8"},   {"u128", "16"}, {"usize", "8"},
  };

  // TODO: Can we maybe get the generic version (atomic_store_n) to work... This
  // would be so much better

  std::string result = prefix;

  auto type_name = operand_type->get_name ();
  if (type_name == "usize" || type_name == "isize")
    {
      rust_sorry_at (
	locus, "atomics are not yet available for size types (usize, isize)");
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

  auto ok = ctx->get_backend ()->function_set_parameters (fndecl, param_vars);
  rust_assert (ok);

  enter_intrinsic_block (ctx, fndecl);

  auto dst = ctx->get_backend ()->var_expression (param_vars[0], Location ());
  TREE_READONLY (dst) = 0;

  auto value = ctx->get_backend ()->var_expression (param_vars[1], Location ());
  auto memorder = make_unsigned_long_tree (ctx, ordering);

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
    = build_fold_addr_expr_loc (Location ().gcc_location (), atomic_store_raw);

  auto store_call
    = ctx->get_backend ()->call_expression (atomic_store,
					    {dst, value, memorder}, nullptr,
					    Location ());
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

  auto ok = ctx->get_backend ()->function_set_parameters (fndecl, param_vars);
  rust_assert (ok);

  enter_intrinsic_block (ctx, fndecl);

  auto src = ctx->get_backend ()->var_expression (param_vars[0], Location ());
  auto memorder = make_unsigned_long_tree (ctx, ordering);

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
    = build_fold_addr_expr_loc (Location ().gcc_location (), atomic_load_raw);

  auto load_call
    = ctx->get_backend ()->call_expression (atomic_load, {src, memorder},
					    nullptr, Location ());
  auto return_statement
    = ctx->get_backend ()->return_statement (fndecl, {load_call}, Location ());

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

  if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  enter_intrinsic_block (ctx, fndecl);

  // BUILTIN unchecked_<op> BODY BEGIN

  auto x = ctx->get_backend ()->var_expression (param_vars[0], Location ());
  auto y = ctx->get_backend ()->var_expression (param_vars[1], Location ());

  auto *monomorphized_type
    = fntype->get_substs ().at (0).get_param_ty ()->resolve ();

  check_for_basic_integer_type ("unchecked operation", fntype->get_locus (),
				monomorphized_type);

  auto expr = build2 (op, TREE_TYPE (x), x, y);
  auto return_statement
    = ctx->get_backend ()->return_statement (fndecl, {expr}, Location ());

  ctx->add_statement (return_statement);

  // BUILTIN unchecked_<op> BODY END

  finalize_intrinsic_block (ctx, fndecl);

  return fndecl;
}

} // namespace Compile
} // namespace Rust
