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
#include "rust-compile-type.h"
#include "rust-compile-fnparam.h"
#include "rust-tree.h"

namespace Rust {
namespace Compile {

static tree
offset_intrinsic_handler (Context *ctx, TyTy::BaseType *fntype);

static const std::map<std::string,
		      std::function<tree (Context *, TyTy::BaseType *)>>
  generic_intrinsics = {{"offset", &offset_intrinsic_handler}};

Intrinsics::Intrinsics (Context *ctx) : ctx (ctx) {}

tree
Intrinsics::compile (TyTy::FnType *fntype)
{
  rust_assert (fntype->get_abi () == ABI::INTRINSIC);

  // https://github.com/rust-lang/rust/blob/master/library/core/src/intrinsics.rs
  // https://github.com/rust-lang/rust/blob/master/compiler/rustc_codegen_llvm/src/intrinsic.rs
  // https://github.com/Rust-GCC/gccrs/issues/658

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

  tree builtin = ctx->get_backend ()->lookup_builtin_by_rust_name (
    fntype->get_identifier ());
  if (builtin != nullptr)
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

  ctx->pop_fn ();
  ctx->push_function (fndecl);

  return fndecl;
}

} // namespace Compile
} // namespace Rust
