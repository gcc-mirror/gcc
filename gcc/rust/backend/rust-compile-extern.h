// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_EXTERN_ITEM
#define RUST_COMPILE_EXTERN_ITEM

#include "rust-compile-base.h"
#include "rust-compile-intrinsic.h"

namespace Rust {
namespace Compile {

class CompileExternItem : public HIRCompileBase,
			  public HIR::HIRExternalItemVisitor
{
public:
  static void compile (HIR::ExternalItem *item, Context *ctx,
		       TyTy::BaseType *concrete = nullptr)
  {
    CompileExternItem compiler (ctx, concrete);
    item->accept_vis (compiler);
  }

  void visit (HIR::ExternalStaticItem &item) override
  {
    TyTy::BaseType *resolved_type = nullptr;
    bool ok = ctx->get_tyctx ()->lookup_type (item.get_mappings ().get_hirid (),
					      &resolved_type);
    rust_assert (ok);

    std::string name = item.get_item_name ();
    // FIXME this is assuming C ABI
    std::string asm_name = name;

    tree type = TyTyResolveCompile::compile (ctx, resolved_type);
    bool is_external = true;
    bool is_hidden = false;
    bool in_unique_section = false;

    Bvariable *static_global
      = ctx->get_backend ()->global_variable (name, asm_name, type, is_external,
					      is_hidden, in_unique_section,
					      item.get_locus ());
    ctx->insert_var_decl (item.get_mappings ().get_hirid (), static_global);
    ctx->push_var (static_global);
  }

  void visit (HIR::ExternalFunctionItem &function) override
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
	  }
      }

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
	      ctx->insert_function_decl (fntype, lookup);

	    return;
	  }
      }

    if (fntype->has_subsititions_defined ())
      {
	// override the Hir Lookups for the substituions in this context
	fntype->override_context ();
      }

    if (fntype->get_abi () == ABI::INTRINSIC)
      {
	Intrinsics compile (ctx);
	tree fndecl = compile.compile (fntype);
	ctx->insert_function_decl (fntype, fndecl);
	return;
      }

    tree compiled_fn_type = TyTyResolveCompile::compile (ctx, fntype);
    std::string ir_symbol_name = function.get_item_name ();
    std::string asm_name = function.get_item_name ();

    const unsigned int flags = Backend::function_is_declaration;
    tree fndecl
      = ctx->get_backend ()->function (compiled_fn_type, ir_symbol_name,
				       asm_name, flags, function.get_locus ());
    TREE_PUBLIC (fndecl) = 1;
    setup_abi_options (fndecl, fntype->get_abi ());

    ctx->insert_function_decl (fntype, fndecl);
  }

private:
  CompileExternItem (Context *ctx, TyTy::BaseType *concrete)
    : HIRCompileBase (ctx), concrete (concrete)
  {}

  TyTy::BaseType *concrete;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_EXTERN_ITEM
