// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-compile-implitem.h"

namespace Rust {
namespace Compile {

void
CompileTraitItem::visit (HIR::TraitItemConst &constant)
{
  rust_assert (concrete != nullptr);
  TyTy::BaseType *resolved_type = concrete;

  const Resolver::CanonicalPath *canonical_path = nullptr;
  bool ok = ctx->get_mappings ()->lookup_canonical_path (
    constant.get_mappings ().get_nodeid (), &canonical_path);
  rust_assert (ok);

  HIR::Expr *const_value_expr = constant.get_expr ().get ();
  tree const_expr
    = compile_constant_item (resolved_type, canonical_path, const_value_expr,
			     constant.get_locus ());
  ctx->push_const (const_expr);
  ctx->insert_const_decl (constant.get_mappings ().get_hirid (), const_expr);

  reference = const_expr;
}

void
CompileTraitItem::visit (HIR::TraitItemFunc &func)
{
  rust_assert (func.has_block_defined ());

  rust_assert (concrete->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *fntype = static_cast<TyTy::FnType *> (concrete);
  fntype->monomorphize ();

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

	  reference = address_expression (lookup, ref_locus);
	  return;
	}
    }

  if (fntype->has_substitutions_defined ())
    {
      // override the Hir Lookups for the substituions in this context
      fntype->override_context ();
    }

  const Resolver::CanonicalPath *canonical_path = nullptr;
  bool ok = ctx->get_mappings ()->lookup_canonical_path (
    func.get_mappings ().get_nodeid (), &canonical_path);
  rust_assert (ok);

  // FIXME: How do we get the proper visibility here?
  auto vis = HIR::Visibility (HIR::Visibility::VisType::PUBLIC);
  HIR::TraitFunctionDecl &function = func.get_decl ();
  tree fndecl
    = compile_function (function.get_function_name ().as_string (),
			function.get_self (), function.get_function_params (),
			function.get_qualifiers (), vis,
			func.get_outer_attrs (), func.get_locus (),
			func.get_block_expr ().get (), canonical_path, fntype);
  reference = address_expression (fndecl, ref_locus);
}

} // namespace Compile
} // namespace Rust
