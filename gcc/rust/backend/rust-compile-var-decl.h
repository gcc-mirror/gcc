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

#ifndef RUST_COMPILE_VAR_DECL
#define RUST_COMPILE_VAR_DECL

#include "rust-compile-base.h"

namespace Rust {
namespace Compile {

class CompileVarDecl : public HIRCompileBase
{
public:
  static ::Bvariable *compile (::Bfunction *fndecl, HIR::Stmt *stmt,
			       Context *ctx)
  {
    CompileVarDecl compiler (ctx, fndecl);
    stmt->accept_vis (compiler);
    rust_assert (compiler.translated != nullptr);
    ctx->insert_var_decl (stmt->get_mappings ().get_hirid (),
			  compiler.translated);
    return compiler.translated;
  }

  virtual ~CompileVarDecl () {}

  void visit (HIR::LetStmt &stmt)
  {
    TyTy::TyBase *resolved_type = nullptr;
    bool ok = ctx->get_tyctx ()->lookup_type (stmt.get_mappings ().get_hirid (),
					      &resolved_type);
    rust_assert (ok);

    ::Btype *translated_type = TyTyResolveCompile::compile (ctx, resolved_type);

    translated = ctx->get_backend ()->local_variable (
      fndecl, stmt.get_pattern ()->as_string (), translated_type,
      NULL /*decl_var*/, false /*address_taken*/, stmt.get_locus ());
  }

private:
  CompileVarDecl (Context *ctx, ::Bfunction *fndecl)
    : HIRCompileBase (ctx), fndecl (fndecl), translated (nullptr)
  {}

  ::Bfunction *fndecl;
  ::Bvariable *translated;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_VAR_DECL
