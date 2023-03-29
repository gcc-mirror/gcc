// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-compile-item.h"
#include "rust-compile-implitem.h"
#include "rust-compile-expr.h"
#include "rust-compile-extern.h"
#include "rust-constexpr.h"

namespace Rust {
namespace Compile {

void
CompileItem::visit (HIR::StaticItem &var)
{
  // have we already compiled this?
  Bvariable *static_decl_ref = nullptr;
  if (ctx->lookup_var_decl (var.get_mappings ().get_hirid (), &static_decl_ref))
    {
      reference
	= ctx->get_backend ()->var_expression (static_decl_ref, ref_locus);
      return;
    }

  TyTy::BaseType *resolved_type = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (var.get_mappings ().get_hirid (),
					    &resolved_type);
  rust_assert (ok);

  tree type = TyTyResolveCompile::compile (ctx, resolved_type);

  const Resolver::CanonicalPath *canonical_path = nullptr;
  ok = ctx->get_mappings ()->lookup_canonical_path (
    var.get_mappings ().get_nodeid (), &canonical_path);
  rust_assert (ok);

  HIR::Expr *const_value_expr = var.get_expr ();
  ctx->push_const_context ();
  tree value = compile_constant_item (ctx, resolved_type, canonical_path,
				      const_value_expr, var.get_locus ());
  ctx->pop_const_context ();

  std::string name = canonical_path->get ();
  std::string asm_name = ctx->mangle_item (resolved_type, *canonical_path);

  bool is_external = false;
  bool is_hidden = false;
  bool in_unique_section = true;

  Bvariable *static_global
    = ctx->get_backend ()->global_variable (name, asm_name, type, is_external,
					    is_hidden, in_unique_section,
					    var.get_locus ());
  ctx->get_backend ()->global_variable_set_init (static_global, value);

  ctx->insert_var_decl (var.get_mappings ().get_hirid (), static_global);
  ctx->push_var (static_global);

  reference = ctx->get_backend ()->var_expression (static_global, ref_locus);
}

void
CompileItem::visit (HIR::ConstantItem &constant)
{
  if (ctx->lookup_const_decl (constant.get_mappings ().get_hirid (),
			      &reference))
    return;

  // resolve the type
  TyTy::BaseType *resolved_type = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_type (constant.get_mappings ().get_hirid (),
				      &resolved_type);
  rust_assert (ok);

  // canonical path
  const Resolver::CanonicalPath *canonical_path = nullptr;
  ok = ctx->get_mappings ()->lookup_canonical_path (
    constant.get_mappings ().get_nodeid (), &canonical_path);
  rust_assert (ok);

  HIR::Expr *const_value_expr = constant.get_expr ();
  ctx->push_const_context ();
  tree const_expr
    = compile_constant_item (ctx, resolved_type, canonical_path,
			     const_value_expr, constant.get_locus ());
  ctx->pop_const_context ();

  ctx->push_const (const_expr);
  ctx->insert_const_decl (constant.get_mappings ().get_hirid (), const_expr);
  reference = const_expr;
}

void
CompileItem::visit (HIR::Function &function)
{
  TyTy::BaseType *fntype_tyty;
  if (!ctx->get_tyctx ()->lookup_type (function.get_mappings ().get_hirid (),
				       &fntype_tyty))
    {
      rust_fatal_error (function.get_locus (),
			"failed to lookup function type");
      return;
    }

  rust_assert (fntype_tyty->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *fntype = static_cast<TyTy::FnType *> (fntype_tyty);
  if (fntype->has_subsititions_defined ())
    {
      // we cant do anything for this only when it is used and a concrete type
      // is given
      if (concrete == nullptr)
	return;
      else
	{
	  rust_assert (concrete->get_kind () == TyTy::TypeKind::FNDEF);
	  fntype = static_cast<TyTy::FnType *> (concrete);
	  fntype->monomorphize ();
	}
    }

  const Resolver::CanonicalPath *canonical_path = nullptr;
  bool ok = ctx->get_mappings ()->lookup_canonical_path (
    function.get_mappings ().get_nodeid (), &canonical_path);
  rust_assert (ok);

  const std::string asm_name = ctx->mangle_item (fntype, *canonical_path);

  // items can be forward compiled which means we may not need to invoke this
  // code. We might also have already compiled this generic function as well.
  tree lookup = NULL_TREE;
  if (ctx->lookup_function_decl (fntype->get_ty_ref (), &lookup,
				 fntype->get_id (), fntype, asm_name))
    {
      // has this been added to the list then it must be finished
      if (ctx->function_completed (lookup))
	{
	  tree dummy = NULL_TREE;
	  if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &dummy))
	    {
	      ctx->insert_function_decl (fntype, lookup);
	    }

	  reference = address_expression (lookup, ref_locus);
	  return;
	}
    }

  if (fntype->has_subsititions_defined ())
    {
      // override the Hir Lookups for the substituions in this context
      fntype->override_context ();
    }

  if (function.get_qualifiers ().is_const ())
    ctx->push_const_context ();

  tree fndecl
    = compile_function (ctx, function.get_function_name (),
			function.get_self_param (),
			function.get_function_params (),
			function.get_qualifiers (), function.get_visibility (),
			function.get_outer_attrs (), function.get_locus (),
			function.get_definition ().get (), canonical_path,
			fntype, function.has_function_return_type ());
  reference = address_expression (fndecl, ref_locus);

  if (function.get_qualifiers ().is_const ())
    ctx->pop_const_context ();
}

void
CompileItem::visit (HIR::ImplBlock &impl_block)
{
  TyTy::BaseType *self_lookup = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (
	impl_block.get_type ()->get_mappings ().get_hirid (), &self_lookup))
    {
      rust_error_at (impl_block.get_locus (), "failed to resolve type of impl");
      return;
    }

  for (auto &impl_item : impl_block.get_impl_items ())
    CompileInherentImplItem::Compile (impl_item.get (), ctx);
}

void
CompileItem::visit (HIR::ExternBlock &extern_block)
{
  for (auto &item : extern_block.get_extern_items ())
    {
      CompileExternItem::compile (item.get (), ctx, concrete);
    }
}

void
CompileItem::visit (HIR::Module &module)
{
  for (auto &item : module.get_items ())
    CompileItem::compile (item.get (), ctx);
}

} // namespace Compile
} // namespace Rust
