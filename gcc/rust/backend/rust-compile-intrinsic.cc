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
#include "fold-const.h"
#include "langhooks.h"
#include "rust-compile-context.h"
#include "rust-compile-type.h"
#include "rust-compile-fnparam.h"
#include "rust-builtins.h"
#include "rust-diagnostics.h"
#include "rust-location.h"
#include "rust-tree.h"
#include "tree-core.h"

namespace Rust {
namespace Compile {

static tree
offset_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype);
static tree
sizeof_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype);
static tree
transmute_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype);
static tree
rotate_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype, tree_code op);
static inline tree
rotate_left_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype)
{
  return rotate_intrinsic_handler (ctx, fntype, LROTATE_EXPR);
}
static inline tree
rotate_right_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype)
{
  return rotate_intrinsic_handler (ctx, fntype, RROTATE_EXPR);
}

static const std::map<std::string,
		      std::function<tree (Context *, TyTy::BaseType *)>>
  generic_intrinsics = {{"offset", &offset_intrinsic_handler},
			{"size_of", &sizeof_intrinsic_handler},
			{"transmute", &transmute_intrinsic_handler},
			{"rotate_left", &rotate_left_intrinsic_handler},
			{"rotate_right", &rotate_right_intrinsic_handler}};

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

static tree
offset_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype_tyty)
{
  rust_assert (fntype_tyty->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *fntype = static_cast<TyTy::FnType *> (fntype_tyty);
  const Resolver::CanonicalPath &canonical_path = fntype->get_ident ().path;

  // items can be forward compiled which means we may not need to invoke this
  // code. We might also have already compiled this generic function as well.
  tree lookup = NULL_TREE;
  if (ctx->lookup_function_decl (fntype->get_ty_ref (), &lookup,
				 fntype->get_id (), fntype))
    {
      // has this been added to the list then it must be finished
      if (ctx->function_completed (lookup))
	{
	  tree dummy = NULL_TREE;
	  if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &dummy))
	    {
	      ctx->insert_function_decl (fntype, lookup);
	    }
	  return lookup;
	}
    }

  if (fntype->has_subsititions_defined ())
    {
      // override the Hir Lookups for the substituions in this context
      fntype->override_context ();
    }

  // offset intrinsic has two params dst pointer and offset isize
  if (fntype->get_params ().size () != 2)
    {
      rust_error_at (fntype->get_ident ().locus,
		     "invalid number of parameters for offset intrinsic");
      return error_mark_node;
    }

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

  // setup the params
  std::vector<Bvariable *> param_vars;
  for (auto &parm : fntype->get_params ())
    {
      auto &referenced_param = parm.first;
      auto &param_tyty = parm.second;
      auto compiled_param_type = TyTyResolveCompile::compile (ctx, param_tyty);

      Location param_locus = referenced_param->get_locus ();
      Bvariable *compiled_param_var
	= CompileFnParam::compile (ctx, fndecl, referenced_param,
				   compiled_param_type, param_locus);

      param_vars.push_back (compiled_param_var);
    }

  auto &dst_param = param_vars.at (0);
  auto &size_param = param_vars.at (1);
  rust_assert (param_vars.size () == 2);
  if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  tree enclosing_scope = NULL_TREE;
  Location start_location = Location ();
  Location end_location = Location ();

  tree code_block = ctx->get_backend ()->block (fndecl, enclosing_scope, {},
						start_location, end_location);
  ctx->push_block (code_block);

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

  tree bind_tree = ctx->pop_block ();

  gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
  DECL_SAVED_TREE (fndecl) = bind_tree;
  ctx->push_function (fndecl);

  return fndecl;
}

static tree
sizeof_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype_tyty)
{
  rust_assert (fntype_tyty->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *fntype = static_cast<TyTy::FnType *> (fntype_tyty);
  const Resolver::CanonicalPath &canonical_path = fntype->get_ident ().path;

  // items can be forward compiled which means we may not need to invoke this
  // code. We might also have already compiled this generic function as well.
  tree lookup = NULL_TREE;
  if (ctx->lookup_function_decl (fntype->get_ty_ref (), &lookup,
				 fntype->get_id (), fntype))
    {
      // has this been added to the list then it must be finished
      if (ctx->function_completed (lookup))
	{
	  tree dummy = NULL_TREE;
	  if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &dummy))
	    {
	      ctx->insert_function_decl (fntype, lookup);
	    }
	  return lookup;
	}
    }

  if (fntype->has_subsititions_defined ())
    {
      // override the Hir Lookups for the substituions in this context
      fntype->override_context ();
    }

  // size_of has _zero_ parameters its parameter is the generic one
  if (fntype->get_params ().size () != 0)
    {
      rust_error_at (fntype->get_ident ().locus,
		     "invalid number of parameters for size of intrinsic");
      return error_mark_node;
    }

  // get the template parameter type tree fn size_of<T>();
  rust_assert (fntype->get_num_substitutions () == 1);
  auto &param_mapping = fntype->get_substs ().at (0);
  const TyTy::ParamType *param_tyty = param_mapping.get_param_ty ();
  TyTy::BaseType *resolved_tyty = param_tyty->resolve ();
  tree template_parameter_type
    = TyTyResolveCompile::compile (ctx, resolved_tyty);

  // build the intrinsic function
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

  tree enclosing_scope = NULL_TREE;
  Location start_location = Location ();
  Location end_location = Location ();

  tree code_block = ctx->get_backend ()->block (fndecl, enclosing_scope, {},
						start_location, end_location);
  ctx->push_block (code_block);

  // BUILTIN size_of FN BODY BEGIN
  tree size_expr = TYPE_SIZE_UNIT (template_parameter_type);
  auto return_statement
    = ctx->get_backend ()->return_statement (fndecl, {size_expr}, Location ());
  ctx->add_statement (return_statement);
  // BUILTIN size_of FN BODY END

  tree bind_tree = ctx->pop_block ();

  gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
  DECL_SAVED_TREE (fndecl) = bind_tree;
  ctx->push_function (fndecl);

  return fndecl;
}

static tree
transmute_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype_tyty)
{
  rust_assert (fntype_tyty->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *fntype = static_cast<TyTy::FnType *> (fntype_tyty);
  const Resolver::CanonicalPath &canonical_path = fntype->get_ident ().path;

  // items can be forward compiled which means we may not need to invoke this
  // code. We might also have already compiled this generic function as well.
  tree lookup = NULL_TREE;
  if (ctx->lookup_function_decl (fntype->get_ty_ref (), &lookup,
				 fntype->get_id (), fntype))
    {
      // has this been added to the list then it must be finished
      if (ctx->function_completed (lookup))
	{
	  tree dummy = NULL_TREE;
	  if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &dummy))
	    {
	      ctx->insert_function_decl (fntype, lookup);
	    }
	  return lookup;
	}
    }

  if (fntype->has_subsititions_defined ())
    {
      // override the Hir Lookups for the substituions in this context
      fntype->override_context ();
    }

  // transmute intrinsic has one parameter
  if (fntype->get_params ().size () != 1)
    {
      rust_error_at (fntype->get_ident ().locus,
		     "invalid number of parameters for transmute intrinsic");
      return error_mark_node;
    }

  // build the intrinsic function
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

  // setup the params
  std::vector<Bvariable *> param_vars;
  std::vector<tree_node *> compiled_types;
  for (auto &parm : fntype->get_params ())
    {
      auto &referenced_param = parm.first;
      auto &param_tyty = parm.second;
      auto compiled_param_type = TyTyResolveCompile::compile (ctx, param_tyty);

      Location param_locus = referenced_param->get_locus ();
      Bvariable *compiled_param_var
	= CompileFnParam::compile (ctx, fndecl, referenced_param,
				   compiled_param_type, param_locus);

      param_vars.push_back (compiled_param_var);
      compiled_types.push_back (compiled_param_type);
    }

  rust_assert (param_vars.size () == 1);
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

  tree enclosing_scope = NULL_TREE;
  Location start_location = Location ();
  Location end_location = Location ();

  tree code_block = ctx->get_backend ()->block (fndecl, enclosing_scope, {},
						start_location, end_location);
  ctx->push_block (code_block);

  // BUILTIN transmute FN BODY BEGIN
  tree result_type_tree = TREE_TYPE (DECL_RESULT (fndecl));
  tree result_expr = error_mark_node;
  if (AGGREGATE_TYPE_P (TREE_TYPE (convert_me_expr)))
    {
      // Return *(orig_type*)&decl.  */
      // tree t = build_fold_addr_expr_loc (location.gcc_location (), this->t_);
      // t = fold_build1_loc (location.gcc_location (), NOP_EXPR,
      //       	       build_pointer_type (this->orig_type_), t);
      // return build_fold_indirect_ref_loc (location.gcc_location (), t);

      // result_expr = fold_build1_loc (Location ().gcc_location (),
      // CONVERT_EXPR,
      //   			     result_type_tree, convert_me_expr);

      tree t = build_fold_addr_expr_loc (Location ().gcc_location (),
					 convert_me_expr);
      t = fold_build1_loc (Location ().gcc_location (), NOP_EXPR,
			   build_pointer_type (target_type_expr), t);
      result_expr
	= build_fold_indirect_ref_loc (Location ().gcc_location (), t);
    }
  else
    {
      result_expr = ctx->get_backend ()->convert_expression (result_type_tree,
							     convert_me_expr,
							     Location ());
    }

  auto return_statement
    = ctx->get_backend ()->return_statement (fndecl, {result_expr},
					     Location ());
  ctx->add_statement (return_statement);
  // BUILTIN transmute FN BODY END

  tree bind_tree = ctx->pop_block ();

  gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
  DECL_SAVED_TREE (fndecl) = bind_tree;
  ctx->push_function (fndecl);

  return fndecl;
}

static tree
rotate_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype_tyty,
			  tree_code op)
{
  rust_assert (fntype_tyty->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *fntype = static_cast<TyTy::FnType *> (fntype_tyty);
  const Resolver::CanonicalPath &canonical_path = fntype->get_ident ().path;

  // items can be forward compiled which means we may not need to invoke this
  // code. We might also have already compiled this generic function as well.
  tree lookup = NULL_TREE;
  if (ctx->lookup_function_decl (fntype->get_ty_ref (), &lookup,
				 fntype->get_id (), fntype))
    {
      // has this been added to the list then it must be finished
      if (ctx->function_completed (lookup))
	{
	  tree dummy = NULL_TREE;
	  if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &dummy))
	    {
	      ctx->insert_function_decl (fntype, lookup);
	    }
	  return lookup;
	}
    }

  if (fntype->has_subsititions_defined ())
    {
      // override the Hir Lookups for the substitutions in this context
      fntype->override_context ();
    }

  // rotate intrinsic has two parameter
  if (fntype->get_params ().size () != 2)
    {
      rust_error_at (fntype->get_ident ().locus,
		     "invalid number of parameters for rotate intrinsic");
      return error_mark_node;
    }

  // build the intrinsic function
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

  // setup the params
  std::vector<Bvariable *> param_vars;
  for (auto &parm : fntype->get_params ())
    {
      auto &referenced_param = parm.first;
      auto &param_tyty = parm.second;
      auto compiled_param_type = TyTyResolveCompile::compile (ctx, param_tyty);

      Location param_locus = referenced_param->get_locus ();
      Bvariable *compiled_param_var
	= CompileFnParam::compile (ctx, fndecl, referenced_param,
				   compiled_param_type, param_locus);

      param_vars.push_back (compiled_param_var);
    }

  auto &x_param = param_vars.at (0);
  auto &y_param = param_vars.at (1);
  rust_assert (param_vars.size () == 2);
  if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  tree enclosing_scope = NULL_TREE;
  Location start_location = Location ();
  Location end_location = Location ();

  tree code_block = ctx->get_backend ()->block (fndecl, enclosing_scope, {},
						start_location, end_location);
  ctx->push_block (code_block);

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

  tree bind_tree = ctx->pop_block ();

  gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
  DECL_SAVED_TREE (fndecl) = bind_tree;
  ctx->push_function (fndecl);

  return fndecl;
}

} // namespace Compile
} // namespace Rust
