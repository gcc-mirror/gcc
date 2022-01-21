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

#ifndef RUST_COMPILE_VAR_DECL
#define RUST_COMPILE_VAR_DECL

#include "rust-compile-base.h"
#include "rust-hir-address-taken.h"

namespace Rust {
namespace Compile {

class CompileVarDecl : public HIRCompileBase
{
  using Rust::Compile::HIRCompileBase::visit;

public:
  static ::Bvariable *compile (tree fndecl, HIR::Stmt *stmt, Context *ctx)
  {
    CompileVarDecl compiler (ctx, fndecl);
    stmt->accept_vis (compiler);
    ctx->insert_var_decl (stmt->get_mappings ().get_hirid (),
			  compiler.compiled_variable);
    return compiler.compiled_variable;
  }

  void visit (HIR::LetStmt &stmt) override
  {
    locus = stmt.get_locus ();
    TyTy::BaseType *resolved_type = nullptr;
    bool ok = ctx->get_tyctx ()->lookup_type (stmt.get_mappings ().get_hirid (),
					      &resolved_type);
    rust_assert (ok);

    address_taken_context->lookup_addess_taken (
      stmt.get_mappings ().get_hirid (), &address_taken);

    translated_type = TyTyResolveCompile::compile (ctx, resolved_type);
    stmt.get_pattern ()->accept_vis (*this);
  }

  void visit (HIR::IdentifierPattern &pattern) override
  {
    if (!pattern.is_mut ())
      translated_type = ctx->get_backend ()->immutable_type (translated_type);

    compiled_variable
      = ctx->get_backend ()->local_variable (fndecl, pattern.get_identifier (),
					     translated_type, NULL /*decl_var*/,
					     address_taken, locus);
  }

  void visit (HIR::WildcardPattern &pattern) override
  {
    translated_type = ctx->get_backend ()->immutable_type (translated_type);
    compiled_variable
      = ctx->get_backend ()->local_variable (fndecl, "_", translated_type,
					     NULL /*decl_var*/, address_taken,
					     locus);
  }

private:
  CompileVarDecl (Context *ctx, tree fndecl)
    : HIRCompileBase (ctx), fndecl (fndecl),
      translated_type (ctx->get_backend ()->error_type ()),
      compiled_variable (ctx->get_backend ()->error_variable ()),
      address_taken (false),
      address_taken_context (Resolver::AddressTakenContext::get ())
  {}

  tree fndecl;
  tree translated_type;
  Location locus;
  Bvariable *compiled_variable;
  bool address_taken;
  const Resolver::AddressTakenContext *address_taken_context;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_VAR_DECL
