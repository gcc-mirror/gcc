// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_IMPLITEM_H
#define RUST_COMPILE_IMPLITEM_H

#include "rust-compile-base.h"
#include "rust-compile-tyty.h"
#include "rust-compile-var-decl.h"
#include "rust-compile-stmt.h"
#include "rust-compile-expr.h"
#include "rust-compile-fnparam.h"

namespace Rust {
namespace Compile {

class CompileInherentImplItem : public HIRCompileBase
{
  using Rust::Compile::HIRCompileBase::visit;

public:
  static Bexpression *Compile (TyTy::BaseType *self, HIR::ImplItem *item,
			       Context *ctx, bool compile_fns,
			       TyTy::BaseType *concrete = nullptr,
			       bool is_query_mode = false,
			       Location ref_locus = Location ())
  {
    CompileInherentImplItem compiler (self, ctx, compile_fns, concrete,
				      ref_locus);
    item->accept_vis (compiler);

    if (is_query_mode
	&& ctx->get_backend ()->is_error_expression (compiler.reference))
      {
	rust_error_at (ref_locus, "failed to compile impl item: %s",
		       item->as_string ().c_str ());
	rust_assert (
	  !ctx->get_backend ()->is_error_expression (compiler.reference));
      }
    return compiler.reference;
  }

  void visit (HIR::ConstantItem &constant) override
  {
    TyTy::BaseType *resolved_type = nullptr;
    bool ok
      = ctx->get_tyctx ()->lookup_type (constant.get_mappings ().get_hirid (),
					&resolved_type);
    rust_assert (ok);

    ::Btype *type = TyTyResolveCompile::compile (ctx, resolved_type);
    Bexpression *value = CompileExpr::Compile (constant.get_expr (), ctx);

    const Resolver::CanonicalPath *canonical_path = nullptr;
    rust_assert (ctx->get_mappings ()->lookup_canonical_path (
      constant.get_mappings ().get_crate_num (),
      constant.get_mappings ().get_nodeid (), &canonical_path));

    std::string ident = canonical_path->get ();
    Bexpression *const_expr = ctx->get_backend ()->named_constant_expression (
      type, constant.get_identifier (), value, constant.get_locus ());

    ctx->push_const (const_expr);
    ctx->insert_const_decl (constant.get_mappings ().get_hirid (), const_expr);

    reference = const_expr;
  }

  void visit (HIR::Function &function) override
  {
    if (!compile_fns)
      return;

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
    Bfunction *lookup = nullptr;
    if (ctx->lookup_function_decl (fntype->get_ty_ref (), &lookup,
				   fntype->get_id (), fntype))
      {
	// has this been added to the list then it must be finished
	if (ctx->function_completed (lookup))
	  {
	    Bfunction *dummy = nullptr;
	    if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &dummy))
	      {
		ctx->insert_function_decl (fntype, lookup);
	      }
	    reference
	      = ctx->get_backend ()->function_code_expression (lookup,
							       ref_locus);
	    return;
	  }
      }

    if (fntype->has_subsititions_defined ())
      {
	// override the Hir Lookups for the substituions in this context
	fntype->override_context ();
      }

    // convert to the actual function type
    ::Btype *compiled_fn_type = TyTyResolveCompile::compile (ctx, fntype);

    unsigned int flags = 0;

    // if its the main fn or pub visibility mark its as DECL_PUBLIC
    // please see https://github.com/Rust-GCC/gccrs/pull/137
    if (function.has_visibility ())
      flags |= Backend::function_is_visible;

    const Resolver::CanonicalPath *canonical_path = nullptr;
    rust_assert (ctx->get_mappings ()->lookup_canonical_path (
      function.get_mappings ().get_crate_num (),
      function.get_mappings ().get_nodeid (), &canonical_path));

    std::string ir_symbol_name
      = canonical_path->get () + fntype->subst_as_string ();
    std::string asm_name
      = ctx->mangle_impl_item (self, fntype, function.get_function_name ());

    Bfunction *fndecl
      = ctx->get_backend ()->function (compiled_fn_type, ir_symbol_name,
				       asm_name, flags, function.get_locus ());
    ctx->insert_function_decl (fntype, fndecl);

    // setup the params
    TyTy::BaseType *tyret = fntype->get_return_type ();
    std::vector<Bvariable *> param_vars;

    if (function.is_method ())
      {
	// insert self
	TyTy::BaseType *self_tyty_lookup = nullptr;
	if (!ctx->get_tyctx ()->lookup_type (
	      function.get_self_param ().get_mappings ().get_hirid (),
	      &self_tyty_lookup))
	  {
	    rust_error_at (function.get_self_param ().get_locus (),
			   "failed to lookup self param type");
	    return;
	  }

	Btype *self_type = TyTyResolveCompile::compile (ctx, self_tyty_lookup);
	if (self_type == nullptr)
	  {
	    rust_error_at (function.get_self_param ().get_locus (),
			   "failed to compile self param type");
	    return;
	  }

	Bvariable *compiled_self_param
	  = CompileSelfParam::compile (ctx, fndecl, function.get_self_param (),
				       self_type,
				       function.get_self_param ().get_locus ());
	if (compiled_self_param == nullptr)
	  {
	    rust_error_at (function.get_self_param ().get_locus (),
			   "failed to compile self param variable");
	    return;
	  }

	param_vars.push_back (compiled_self_param);
	ctx->insert_var_decl (
	  function.get_self_param ().get_mappings ().get_hirid (),
	  compiled_self_param);
      }

    // offset from + 1 for the TyTy::FnType being used when this is a method to
    // skip over Self on the FnType
    size_t i = function.is_method () ? 1 : 0;
    for (auto referenced_param : function.get_function_params ())
      {
	auto tyty_param = fntype->param_at (i);
	auto param_tyty = tyty_param.second;

	auto compiled_param_type
	  = TyTyResolveCompile::compile (ctx, param_tyty);
	if (compiled_param_type == nullptr)
	  {
	    rust_error_at (referenced_param.get_locus (),
			   "failed to compile parameter type");
	    return;
	  }

	Location param_locus
	  = ctx->get_mappings ()->lookup_location (param_tyty->get_ref ());
	Bvariable *compiled_param_var
	  = CompileFnParam::compile (ctx, fndecl, &referenced_param,
				     compiled_param_type, param_locus);
	if (compiled_param_var == nullptr)
	  {
	    rust_error_at (param_locus, "Failed to compile parameter variable");
	    return;
	  }

	param_vars.push_back (compiled_param_var);

	ctx->insert_var_decl (referenced_param.get_mappings ().get_hirid (),
			      compiled_param_var);
	i++;
      }

    if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
      {
	rust_fatal_error (function.get_locus (),
			  "failed to setup parameter variables");
	return;
      }

    // lookup locals
    auto block_expr = function.get_definition ().get ();
    auto body_mappings = block_expr->get_mappings ();

    Resolver::Rib *rib = nullptr;
    if (!ctx->get_resolver ()->find_name_rib (body_mappings.get_nodeid (),
					      &rib))
      {
	rust_fatal_error (function.get_locus (),
			  "failed to setup locals per block");
	return;
      }

    std::vector<Bvariable *> locals;
    bool ok = compile_locals_for_block (*rib, fndecl, locals);
    rust_assert (ok);

    Bblock *enclosing_scope = NULL;
    HIR::BlockExpr *function_body = function.get_definition ().get ();
    Location start_location = function_body->get_locus ();
    Location end_location = function_body->get_closing_locus ();

    Bblock *code_block
      = ctx->get_backend ()->block (fndecl, enclosing_scope, locals,
				    start_location, end_location);
    ctx->push_block (code_block);

    Bvariable *return_address = nullptr;
    if (function.has_function_return_type ())
      {
	Btype *return_type = TyTyResolveCompile::compile (ctx, tyret);

	bool address_is_taken = false;
	Bstatement *ret_var_stmt = nullptr;

	return_address = ctx->get_backend ()->temporary_variable (
	  fndecl, code_block, return_type, NULL, address_is_taken,
	  function.get_locus (), &ret_var_stmt);

	ctx->add_statement (ret_var_stmt);
      }

    ctx->push_fn (fndecl, return_address);

    compile_function_body (fndecl, function.get_definition (),
			   function.has_function_return_type ());

    ctx->pop_block ();
    auto body = ctx->get_backend ()->block_statement (code_block);
    if (!ctx->get_backend ()->function_set_body (fndecl, body))
      {
	rust_error_at (function.get_locus (), "failed to set body to function");
	return;
      }

    ctx->pop_fn ();
    ctx->push_function (fndecl);

    reference
      = ctx->get_backend ()->function_code_expression (fndecl, ref_locus);
  }

private:
  CompileInherentImplItem (TyTy::BaseType *self, Context *ctx, bool compile_fns,
			   TyTy::BaseType *concrete, Location ref_locus)
    : HIRCompileBase (ctx), self (self), compile_fns (compile_fns),
      concrete (concrete), reference (ctx->get_backend ()->error_expression ()),
      ref_locus (ref_locus)
  {}

  TyTy::BaseType *self;
  bool compile_fns;
  TyTy::BaseType *concrete;
  Bexpression *reference;
  Location ref_locus;
};

class CompileTraitItem : public HIRCompileBase
{
  using Rust::Compile::HIRCompileBase::visit;

public:
  static Bexpression *Compile (TyTy::BaseType *self, HIR::TraitItem *item,
			       Context *ctx, TyTy::BaseType *concrete,
			       bool is_query_mode = false,
			       Location ref_locus = Location ())
  {
    CompileTraitItem compiler (self, ctx, concrete, ref_locus);
    item->accept_vis (compiler);

    if (is_query_mode
	&& ctx->get_backend ()->is_error_expression (compiler.reference))
      {
	rust_error_at (ref_locus, "failed to compile trait item: %s",
		       item->as_string ().c_str ());
	rust_assert (
	  !ctx->get_backend ()->is_error_expression (compiler.reference));
      }
    return compiler.reference;
  }

  void visit (HIR::TraitItemConst &constant) override
  {
    rust_assert (concrete != nullptr);
    TyTy::BaseType *resolved_type = concrete;

    ::Btype *type = TyTyResolveCompile::compile (ctx, resolved_type);
    Bexpression *value
      = CompileExpr::Compile (constant.get_expr ().get (), ctx);

    const Resolver::CanonicalPath *canonical_path = nullptr;
    rust_assert (ctx->get_mappings ()->lookup_canonical_path (
      constant.get_mappings ().get_crate_num (),
      constant.get_mappings ().get_nodeid (), &canonical_path));

    std::string ident = canonical_path->get ();
    Bexpression *const_expr = ctx->get_backend ()->named_constant_expression (
      type, constant.get_name (), value, constant.get_locus ());

    ctx->push_const (const_expr);
    ctx->insert_const_decl (constant.get_mappings ().get_hirid (), const_expr);

    reference = const_expr;
  }

  void visit (HIR::TraitItemFunc &func) override
  {
    rust_assert (func.has_block_defined ());

    rust_assert (concrete->get_kind () == TyTy::TypeKind::FNDEF);
    TyTy::FnType *fntype = static_cast<TyTy::FnType *> (concrete);

    // items can be forward compiled which means we may not need to invoke this
    // code. We might also have already compiled this generic function as well.
    Bfunction *lookup = nullptr;
    if (ctx->lookup_function_decl (fntype->get_ty_ref (), &lookup,
				   fntype->get_id (), fntype))
      {
	// has this been added to the list then it must be finished
	if (ctx->function_completed (lookup))
	  {
	    Bfunction *dummy = nullptr;
	    if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &dummy))
	      {
		ctx->insert_function_decl (fntype, lookup);
	      }
	    reference
	      = ctx->get_backend ()->function_code_expression (lookup,
							       ref_locus);
	    return;
	  }
      }

    if (fntype->has_subsititions_defined ())
      {
	// override the Hir Lookups for the substituions in this context
	fntype->override_context ();
      }

    // convert to the actual function type
    ::Btype *compiled_fn_type = TyTyResolveCompile::compile (ctx, fntype);

    HIR::TraitFunctionDecl &function = func.get_decl ();
    unsigned int flags = 0;

    const Resolver::CanonicalPath *canonical_path = nullptr;
    rust_assert (ctx->get_mappings ()->lookup_canonical_path (
      func.get_mappings ().get_crate_num (), func.get_mappings ().get_nodeid (),
      &canonical_path));

    std::string fn_identifier = canonical_path->get ();
    std::string asm_name = ctx->mangle_item (fntype, *canonical_path);

    Bfunction *fndecl
      = ctx->get_backend ()->function (compiled_fn_type, fn_identifier,
				       asm_name, flags, func.get_locus ());
    ctx->insert_function_decl (fntype, fndecl);

    // setup the params
    TyTy::BaseType *tyret = fntype->get_return_type ();
    std::vector<Bvariable *> param_vars;

    if (function.is_method ())
      {
	// insert self
	TyTy::BaseType *self_tyty_lookup = nullptr;
	if (!ctx->get_tyctx ()->lookup_type (
	      function.get_self ().get_mappings ().get_hirid (),
	      &self_tyty_lookup))
	  {
	    rust_error_at (function.get_self ().get_locus (),
			   "failed to lookup self param type");
	    return;
	  }

	Btype *self_type = TyTyResolveCompile::compile (ctx, self_tyty_lookup);
	if (self_type == nullptr)
	  {
	    rust_error_at (function.get_self ().get_locus (),
			   "failed to compile self param type");
	    return;
	  }

	Bvariable *compiled_self_param
	  = CompileSelfParam::compile (ctx, fndecl, function.get_self (),
				       self_type,
				       function.get_self ().get_locus ());
	if (compiled_self_param == nullptr)
	  {
	    rust_error_at (function.get_self ().get_locus (),
			   "failed to compile self param variable");
	    return;
	  }

	param_vars.push_back (compiled_self_param);
	ctx->insert_var_decl (function.get_self ().get_mappings ().get_hirid (),
			      compiled_self_param);
      }

    // offset from + 1 for the TyTy::FnType being used when this is a method to
    // skip over Self on the FnType
    size_t i = function.is_method () ? 1 : 0;
    for (auto referenced_param : function.get_function_params ())
      {
	auto tyty_param = fntype->param_at (i);
	auto param_tyty = tyty_param.second;

	auto compiled_param_type
	  = TyTyResolveCompile::compile (ctx, param_tyty);
	if (compiled_param_type == nullptr)
	  {
	    rust_error_at (referenced_param.get_locus (),
			   "failed to compile parameter type");
	    return;
	  }

	Location param_locus
	  = ctx->get_mappings ()->lookup_location (param_tyty->get_ref ());
	Bvariable *compiled_param_var
	  = CompileFnParam::compile (ctx, fndecl, &referenced_param,
				     compiled_param_type, param_locus);
	if (compiled_param_var == nullptr)
	  {
	    rust_error_at (param_locus, "Failed to compile parameter variable");
	    return;
	  }

	param_vars.push_back (compiled_param_var);

	ctx->insert_var_decl (referenced_param.get_mappings ().get_hirid (),
			      compiled_param_var);
	i++;
      }

    if (!ctx->get_backend ()->function_set_parameters (fndecl, param_vars))
      {
	rust_fatal_error (func.get_locus (),
			  "failed to setup parameter variables");
	return;
      }

    // lookup locals
    auto block_expr = func.get_block_expr ().get ();
    auto body_mappings = block_expr->get_mappings ();

    Resolver::Rib *rib = nullptr;
    if (!ctx->get_resolver ()->find_name_rib (body_mappings.get_nodeid (),
					      &rib))
      {
	rust_fatal_error (func.get_locus (),
			  "failed to setup locals per block");
	return;
      }

    std::vector<Bvariable *> locals;
    bool ok = compile_locals_for_block (*rib, fndecl, locals);
    rust_assert (ok);

    Bblock *enclosing_scope = NULL;
    HIR::BlockExpr *function_body = func.get_block_expr ().get ();
    Location start_location = function_body->get_locus ();
    Location end_location = function_body->get_closing_locus ();

    Bblock *code_block
      = ctx->get_backend ()->block (fndecl, enclosing_scope, locals,
				    start_location, end_location);
    ctx->push_block (code_block);

    Bvariable *return_address = nullptr;
    if (function.has_return_type ())
      {
	Btype *return_type = TyTyResolveCompile::compile (ctx, tyret);

	bool address_is_taken = false;
	Bstatement *ret_var_stmt = nullptr;

	return_address = ctx->get_backend ()->temporary_variable (
	  fndecl, code_block, return_type, NULL, address_is_taken,
	  func.get_locus (), &ret_var_stmt);

	ctx->add_statement (ret_var_stmt);
      }

    ctx->push_fn (fndecl, return_address);

    compile_function_body (fndecl, func.get_block_expr (),
			   function.has_return_type ());

    ctx->pop_block ();
    auto body = ctx->get_backend ()->block_statement (code_block);
    if (!ctx->get_backend ()->function_set_body (fndecl, body))
      {
	rust_error_at (func.get_locus (), "failed to set body to function");
	return;
      }

    ctx->pop_fn ();
    ctx->push_function (fndecl);

    reference
      = ctx->get_backend ()->function_code_expression (fndecl, ref_locus);
  }

private:
  CompileTraitItem (TyTy::BaseType *self, Context *ctx,
		    TyTy::BaseType *concrete, Location ref_locus)
    : HIRCompileBase (ctx), self (self), concrete (concrete),
      reference (ctx->get_backend ()->error_expression ()),
      ref_locus (ref_locus)
  {}

  TyTy::BaseType *self;
  TyTy::BaseType *concrete;
  Bexpression *reference;
  Location ref_locus;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_IMPLITEM_H
