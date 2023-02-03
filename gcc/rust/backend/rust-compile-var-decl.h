// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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
#include "rust-hir-visitor.h"

namespace Rust {
namespace Compile {

class CompileVarDecl : public HIRCompileBase, public HIR::HIRPatternVisitor
{
  using HIR::HIRPatternVisitor::visit;

public:
  static void compile (tree fndecl, tree translated_type, HIR::Pattern *pattern,
		       std::vector<Bvariable *> &locals, Context *ctx)
  {
    CompileVarDecl compiler (ctx, fndecl, translated_type, locals);
    pattern->accept_vis (compiler);
  }

  void visit (HIR::IdentifierPattern &pattern) override
  {
    if (!pattern.is_mut ())
      translated_type = ctx->get_backend ()->immutable_type (translated_type);

    Bvariable *var
      = ctx->get_backend ()->local_variable (fndecl, pattern.get_identifier (),
					     translated_type, NULL /*decl_var*/,
					     pattern.get_locus ());

    HirId stmt_id = pattern.get_pattern_mappings ().get_hirid ();
    ctx->insert_var_decl (stmt_id, var);

    locals.push_back (var);
  }

  // Empty visit for unused Pattern HIR nodes.
  void visit (HIR::LiteralPattern &) override {}
  void visit (HIR::PathInExpression &) override {}
  void visit (HIR::QualifiedPathInExpression &) override {}
  void visit (HIR::RangePattern &) override {}
  void visit (HIR::ReferencePattern &) override {}
  void visit (HIR::SlicePattern &) override {}
  void visit (HIR::StructPattern &) override {}
  void visit (HIR::TuplePattern &) override {}
  void visit (HIR::TupleStructPattern &) override {}
  void visit (HIR::WildcardPattern &) override {}

private:
  CompileVarDecl (Context *ctx, tree fndecl, tree translated_type,
		  std::vector<Bvariable *> &locals)
    : HIRCompileBase (ctx), fndecl (fndecl), translated_type (translated_type),
      locals (locals)
  {}

  tree fndecl;
  tree translated_type;

  std::vector<Bvariable *> &locals;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_VAR_DECL
