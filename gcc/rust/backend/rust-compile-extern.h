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

#ifndef RUST_COMPILE_EXTERN_ITEM
#define RUST_COMPILE_EXTERN_ITEM

#include "rust-compile-base.h"
#include "rust-compile-intrinsic.h"
#include "rust-compile-type.h"
#include "rust-diagnostics.h"
#include "rust-hir-full-decls.h"

namespace Rust {
namespace Compile {

class CompileExternItem : public HIRCompileBase,
			  public HIR::HIRExternalItemVisitor
{
public:
  static tree compile (HIR::ExternalItem *item, Context *ctx,
		       TyTy::BaseType *concrete = nullptr,
		       location_t ref_locus = UNDEF_LOCATION)
  {
    CompileExternItem compiler (ctx, concrete, ref_locus);
    item->accept_vis (compiler);
    return compiler.reference;
  }

  void visit (HIR::ExternalStaticItem &item) override
  {
    // check if its already been compiled
    Bvariable *lookup = Bvariable::error_variable ();
    if (ctx->lookup_var_decl (item.get_mappings ().get_hirid (), &lookup))
      {
	reference = Backend::var_expression (lookup, ref_locus);
	return;
      }

    TyTy::BaseType *resolved_type = nullptr;
    bool ok = ctx->get_tyctx ()->lookup_type (item.get_mappings ().get_hirid (),
					      &resolved_type);
    rust_assert (ok);

    std::string name = item.get_item_name ().as_string ();
    // FIXME this is assuming C ABI
    std::string asm_name = name;

    tree type = TyTyResolveCompile::compile (ctx, resolved_type);
    bool is_external = true;
    bool is_hidden = false;
    bool in_unique_section = false;

    Bvariable *static_global
      = Backend::global_variable (name, asm_name, type, is_external, is_hidden,
				  in_unique_section, item.get_locus ());
    ctx->insert_var_decl (item.get_mappings ().get_hirid (), static_global);
    ctx->push_var (static_global);

    reference = Backend::var_expression (static_global, ref_locus);
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
    if (fntype->has_substitutions_defined ())
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
	reference = address_expression (lookup, ref_locus);
	return;
      }

    if (fntype->has_substitutions_defined ())
      // override the HIR lookups for the substitutions in this context
      fntype->override_context ();

    if (fntype->get_abi () == ABI::INTRINSIC)
      {
	Intrinsics compile (ctx);
	tree fndecl = compile.compile (fntype);
	ctx->insert_function_decl (fntype, fndecl);
	return;
      }

    tree compiled_fn_type = TyTyResolveCompile::compile (ctx, fntype);
    std::string ir_symbol_name = function.get_item_name ().as_string ();
    std::string asm_name = function.get_item_name ().as_string ();
    if (fntype->get_abi () == ABI::RUST)
      {
	// then we need to get the canonical path of it and mangle it
	auto canonical_path = ctx->get_mappings ().lookup_canonical_path (
	  function.get_mappings ().get_nodeid ());

	ir_symbol_name = canonical_path->get () + fntype->subst_as_string ();
	asm_name = ctx->mangle_item (fntype, *canonical_path);
      }

    const unsigned int flags = Backend::function_is_declaration;
    tree fndecl = Backend::function (compiled_fn_type, ir_symbol_name, asm_name,
				     flags, function.get_locus ());
    TREE_PUBLIC (fndecl) = 1;
    setup_abi_options (fndecl, fntype->get_abi ());

    ctx->insert_function_decl (fntype, fndecl);

    reference = address_expression (fndecl, ref_locus);
  }

  void visit (HIR::ExternalTypeItem &type) override
  {
    rust_sorry_at (type.get_locus (), "extern types are not supported yet");
  }

private:
  CompileExternItem (Context *ctx, TyTy::BaseType *concrete,
		     location_t ref_locus)
    : HIRCompileBase (ctx), concrete (concrete), reference (error_mark_node),
      ref_locus (ref_locus)
  {}

  TyTy::BaseType *concrete;
  tree reference;
  location_t ref_locus;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_EXTERN_ITEM
