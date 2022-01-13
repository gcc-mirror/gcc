// Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_STMT
#define RUST_COMPILE_STMT

#include "rust-compile-base.h"
#include "rust-compile-tyty.h"
#include "rust-compile-expr.h"

namespace Rust {
namespace Compile {

class CompileStmt : public HIRCompileBase
{
  using Rust::Compile::HIRCompileBase::visit;

public:
  static tree Compile (HIR::Stmt *stmt, Context *ctx)
  {
    CompileStmt compiler (ctx);
    stmt->accept_vis (compiler);
    return compiler.translated;
  }

  void visit (HIR::ExprStmtWithBlock &stmt) override
  {
    translated = CompileExpr::Compile (stmt.get_expr (), ctx);
  }

  void visit (HIR::ExprStmtWithoutBlock &stmt) override
  {
    translated = CompileExpr::Compile (stmt.get_expr (), ctx);
  }

  void visit (HIR::ConstantItem &constant) override
  {
    TyTy::BaseType *resolved_type = nullptr;
    bool ok
      = ctx->get_tyctx ()->lookup_type (constant.get_mappings ().get_hirid (),
					&resolved_type);
    rust_assert (ok);

    tree type = TyTyResolveCompile::compile (ctx, resolved_type);
    tree value = CompileExpr::Compile (constant.get_expr (), ctx);

    const Resolver::CanonicalPath *canonical_path = nullptr;
    ok = ctx->get_mappings ()->lookup_canonical_path (
      constant.get_mappings ().get_crate_num (),
      constant.get_mappings ().get_nodeid (), &canonical_path);
    rust_assert (ok);

    std::string ident = canonical_path->get ();
    tree const_expr
      = ctx->get_backend ()->named_constant_expression (type, ident, value,
							constant.get_locus ());

    ctx->push_const (const_expr);
    ctx->insert_const_decl (constant.get_mappings ().get_hirid (), const_expr);

    translated = const_expr;
  }

  void visit (HIR::LetStmt &stmt) override
  {
    // nothing to do
    if (!stmt.has_init_expr ())
      return;

    TyTy::BaseType *ty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (stmt.get_mappings ().get_hirid (),
					 &ty))
      {
	// FIXME this should be an assertion instead
	rust_fatal_error (stmt.get_locus (),
			  "failed to lookup variable declaration type");
	return;
      }

    Bvariable *var = nullptr;
    if (!ctx->lookup_var_decl (stmt.get_mappings ().get_hirid (), &var))
      {
	// FIXME this should be an assertion instead and use error mark node
	rust_fatal_error (stmt.get_locus (),
			  "failed to lookup compiled variable declaration");
	return;
      }

    tree init = CompileExpr::Compile (stmt.get_init_expr (), ctx);
    // FIXME use error_mark_node, check that CompileExpr returns error_mark_node
    // on failure and make this an assertion
    if (init == nullptr)
      return;

    TyTy::BaseType *actual = nullptr;
    bool ok = ctx->get_tyctx ()->lookup_type (
      stmt.get_init_expr ()->get_mappings ().get_hirid (), &actual);
    rust_assert (ok);

    Location lvalue_locus = stmt.get_pattern ()->get_locus ();
    Location rvalue_locus = stmt.get_init_expr ()->get_locus ();
    TyTy::BaseType *expected = ty;
    init = coercion_site (init, actual, expected, lvalue_locus, rvalue_locus);

    auto fnctx = ctx->peek_fn ();
    if (ty->is_unit ())
      {
	tree expr_stmt
	  = ctx->get_backend ()->expression_statement (fnctx.fndecl, init);
	ctx->add_statement (expr_stmt);
      }
    else
      {
	auto s = ctx->get_backend ()->init_statement (fnctx.fndecl, var, init);
	ctx->add_statement (s);
      }
  }

private:
  CompileStmt (Context *ctx) : HIRCompileBase (ctx), translated (nullptr) {}

  tree translated;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_STMT
