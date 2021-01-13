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

#ifndef RUST_COMPILE_STMT
#define RUST_COMPILE_STMT

#include "rust-compile-base.h"
#include "rust-compile-tyty.h"
#include "rust-compile-expr.h"

namespace Rust {
namespace Compile {

class CompileStmt : public HIRCompileBase
{
public:
  static void Compile (HIR::Stmt *stmt, Context *ctx)
  {
    CompileStmt compiler (ctx);
    stmt->accept_vis (compiler);
    rust_assert (compiler.ok);
  }

  virtual ~CompileStmt () {}

  void visit (HIR::ExprStmtWithBlock &stmt)
  {
    ok = true;
    auto translated = CompileExpr::Compile (stmt.get_expr (), ctx);

    // these can be null
    if (translated == nullptr)
      return;

    gcc_unreachable ();
  }

  void visit (HIR::ExprStmtWithoutBlock &stmt)
  {
    ok = true;
    auto translated = CompileExpr::Compile (stmt.get_expr (), ctx);

    // these can be null
    if (translated == nullptr)
      return;

    gcc_unreachable ();
  }

  void visit (HIR::LetStmt &stmt)
  {
    // marks that the statement has been looked at
    ok = true;

    // nothing to do
    if (!stmt.has_init_expr ())
      return;

    TyTy::TyBase *ty = nullptr;
    if (!ctx->get_tyctx ()->lookup_type (stmt.get_mappings ().get_hirid (),
					 &ty))
      {
	rust_fatal_error (stmt.get_locus (), "failed to lookup var decl type");
	return;
      }

    // there is an ICE in GCC for void_node
    if (ty->get_kind () == TyTy::TypeKind::UNIT)
      return;

    Bvariable *var = nullptr;
    if (!ctx->lookup_var_decl (stmt.get_mappings ().get_hirid (), &var))
      {
	rust_fatal_error (stmt.get_locus (),
			  "failed to lookup compiled variable decl");
	return;
      }

    Bexpression *init = CompileExpr::Compile (stmt.get_init_expr (), ctx);

    auto fnctx = ctx->peek_fn ();
    auto s = ctx->get_backend ()->init_statement (fnctx.fndecl, var, init);
    ctx->add_statement (s);
  }

private:
  CompileStmt (Context *ctx) : HIRCompileBase (ctx), ok (false) {}

  bool ok;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_STMT
