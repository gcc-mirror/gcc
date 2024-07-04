// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-compile-pattern.h"
#include "rust-compile-stmt.h"
#include "rust-compile-expr.h"
#include "rust-compile-type.h"
#include "rust-compile-var-decl.h"

namespace Rust {
namespace Compile {

CompileStmt::CompileStmt (Context *ctx)
  : HIRCompileBase (ctx), translated (nullptr)
{}

tree
CompileStmt::Compile (HIR::Stmt *stmt, Context *ctx)
{
  CompileStmt compiler (ctx);
  stmt->accept_vis (compiler);
  return compiler.translated;
}

void
CompileStmt::visit (HIR::ExprStmt &stmt)
{
  translated = CompileExpr::Compile (stmt.get_expr ().get (), ctx);
}

void
CompileStmt::visit (HIR::LetStmt &stmt)
{
  HIR::Pattern &stmt_pattern = *stmt.get_pattern ();
  HirId stmt_id = stmt_pattern.get_mappings ().get_hirid ();

  TyTy::BaseType *ty = nullptr;
  if (!ctx->get_tyctx ()->lookup_type (stmt_id, &ty))
    {
      // FIXME this should be an assertion instead
      rust_fatal_error (stmt.get_locus (),
			"failed to lookup variable declaration type");
      return;
    }

  // setup var decl nodes
  fncontext fnctx = ctx->peek_fn ();
  tree fndecl = fnctx.fndecl;
  tree translated_type = TyTyResolveCompile::compile (ctx, ty);
  CompileVarDecl::compile (fndecl, translated_type, &stmt_pattern, ctx);

  // nothing to do
  if (!stmt.has_init_expr ())
    return;

  tree init = CompileExpr::Compile (stmt.get_init_expr ().get (), ctx);
  // FIXME use error_mark_node, check that CompileExpr returns error_mark_node
  // on failure and make this an assertion
  if (init == nullptr)
    return;

  TyTy::BaseType *actual = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    stmt.get_init_expr ()->get_mappings ().get_hirid (), &actual);
  rust_assert (ok);

  location_t lvalue_locus = stmt.get_pattern ()->get_locus ();
  location_t rvalue_locus = stmt.get_init_expr ()->get_locus ();
  TyTy::BaseType *expected = ty;
  init = coercion_site (stmt.get_mappings ().get_hirid (), init, actual,
			expected, lvalue_locus, rvalue_locus);

  CompilePatternLet::Compile (&stmt_pattern, init, ty, rvalue_locus, ctx);
}

} // namespace Compile
} // namespace Rust
