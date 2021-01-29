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
public:
  static void Compile (HIR::Type *base, HIR::InherentImplItem *item,
		       Context *ctx, bool compile_fns)
  {
    CompileInherentImplItem compiler (base, ctx, compile_fns);
    item->accept_vis (compiler);
  }

  void visit (HIR::ConstantItem &constant)
  {
    TyTy::TyBase *resolved_type = nullptr;
    bool ok
      = ctx->get_tyctx ()->lookup_type (constant.get_mappings ().get_hirid (),
					&resolved_type);
    rust_assert (ok);

    ::Btype *type = TyTyResolveCompile::compile (ctx, resolved_type);
    Bexpression *value = CompileExpr::Compile (constant.get_expr (), ctx);

    std::string ident = base->as_string () + "::" + constant.get_identifier ();
    Bexpression *const_expr = ctx->get_backend ()->named_constant_expression (
      type, constant.get_identifier (), value, constant.get_locus ());

    ctx->push_const (const_expr);
    ctx->insert_const_decl (constant.get_mappings ().get_hirid (), const_expr);
  }

  void visit (HIR::Function &function)
  {
    if (!compile_fns)
      return;

    // items can be forward compiled which means we may not need to invoke this
    // code
    Bfunction *lookup = nullptr;
    if (ctx->lookup_function_decl (function.get_mappings ().get_hirid (),
				   &lookup))
      {
	// has this been added to the list then it must be finished
	if (ctx->function_completed (lookup))
	  return;
      }

    TyTy::TyBase *fntype_tyty;
    if (!ctx->get_tyctx ()->lookup_type (function.get_mappings ().get_hirid (),
					 &fntype_tyty))
      {
	rust_fatal_error (function.locus, "failed to lookup function type");
	return;
      }

    if (fntype_tyty->get_kind () != TyTy::TypeKind::FNDEF)
      {
	rust_error_at (function.get_locus (), "invalid TyTy for function item");
	return;
      }

    TyTy::FnType *fntype = (TyTy::FnType *) fntype_tyty;
    // convert to the actual function type
    ::Btype *compiled_fn_type = TyTyResolveCompile::compile (ctx, fntype);

    unsigned int flags = 0;
    bool is_main_fn = function.function_name.compare ("main") == 0;

    std::string fn_identifier
      = base->as_string () + "::" + function.function_name;

    // if its the main fn or pub visibility mark its as DECL_PUBLIC
    // please see https://github.com/Rust-GCC/gccrs/pull/137
    if (is_main_fn || function.has_visibility ())
      flags |= Backend::function_is_visible;

    std::string asm_name = fn_identifier;
    if (!is_main_fn)
      {
	// FIXME need name mangling
	asm_name = "__" + function.function_name;
      }

    Bfunction *fndecl
      = ctx->get_backend ()->function (compiled_fn_type, fn_identifier,
				       asm_name, flags, function.get_locus ());
    ctx->insert_function_decl (function.get_mappings ().get_hirid (), fndecl);

    // setup the params

    TyTy::TyBase *tyret = fntype->return_type ();
    std::vector<Bvariable *> param_vars;

    size_t i = 0;
    for (auto &it : fntype->get_params ())
      {
	HIR::FunctionParam &referenced_param = function.function_params.at (i);
	auto param_tyty = it.second;
	auto compiled_param_type
	  = TyTyResolveCompile::compile (ctx, param_tyty);

	Location param_locus
	  = ctx->get_mappings ()->lookup_location (param_tyty->get_ref ());
	Bvariable *compiled_param_var
	  = CompileFnParam::compile (ctx, fndecl, &referenced_param,
				     compiled_param_type, param_locus);
	if (compiled_param_var == nullptr)
	  {
	    rust_error_at (param_locus, "failed to compile parameter variable");
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
    auto block_expr = function.function_body.get ();
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
    rib->iterate_decls ([&] (NodeId n, Location) mutable -> bool {
      Resolver::Definition d;
      bool ok = ctx->get_resolver ()->lookup_definition (n, &d);
      rust_assert (ok);

      HIR::Stmt *decl = nullptr;
      ok = ctx->get_mappings ()->resolve_nodeid_to_stmt (d.parent, &decl);
      rust_assert (ok);

      Bvariable *compiled = CompileVarDecl::compile (fndecl, decl, ctx);
      locals.push_back (compiled);

      return true;
    });

    bool toplevel_item
      = function.get_mappings ().get_local_defid () != UNKNOWN_LOCAL_DEFID;
    Bblock *enclosing_scope
      = toplevel_item ? NULL : ctx->peek_enclosing_scope ();

    HIR::BlockExpr *function_body = function.function_body.get ();
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

    // compile the block
    function_body->iterate_stmts ([&] (HIR::Stmt *s) mutable -> bool {
      CompileStmt::Compile (s, ctx);
      return true;
    });

    if (function_body->has_expr () && function_body->tail_expr_reachable ())
      {
	// the previous passes will ensure this is a valid return
	// dead code elimination should remove any bad trailing expressions
	Bexpression *compiled_expr
	  = CompileExpr::Compile (function_body->expr.get (), ctx);
	rust_assert (compiled_expr != nullptr);

	auto fncontext = ctx->peek_fn ();

	std::vector<Bexpression *> retstmts;
	retstmts.push_back (compiled_expr);
	auto s = ctx->get_backend ()->return_statement (
	  fncontext.fndecl, retstmts, function_body->expr->get_locus_slow ());
	ctx->add_statement (s);
      }

    ctx->pop_block ();
    auto body = ctx->get_backend ()->block_statement (code_block);
    if (!ctx->get_backend ()->function_set_body (fndecl, body))
      {
	rust_error_at (function.get_locus (), "failed to set body to function");
	return;
      }

    ctx->pop_fn ();

    ctx->push_function (fndecl);
  }

private:
  CompileInherentImplItem (HIR::Type *base, Context *ctx, bool compile_fns)
    : HIRCompileBase (ctx), base (base), compile_fns (compile_fns)
  {}

  HIR::Type *base;
  bool compile_fns;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_IMPLITEM_H
