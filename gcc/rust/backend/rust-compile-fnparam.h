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

#ifndef RUST_COMPILE_FNPARAM
#define RUST_COMPILE_FNPARAM

#include "rust-compile-base.h"

namespace Rust {
namespace Compile {

class CompileFnParam : public HIRCompileBase, public HIR::HIRPatternVisitor
{
public:
  static Bvariable *compile (Context *ctx, tree fndecl,
			     HIR::FunctionParam *param, tree decl_type,
			     Location locus)
  {
    CompileFnParam compiler (ctx, fndecl, decl_type, locus);
    param->get_param_name ()->accept_vis (compiler);
    return compiler.compiled_param;
  }

  void visit (HIR::IdentifierPattern &pattern) override
  {
    if (!pattern.is_mut ())
      decl_type = ctx->get_backend ()->immutable_type (decl_type);

    compiled_param
      = ctx->get_backend ()->parameter_variable (fndecl,
						 pattern.get_identifier (),
						 decl_type, locus);
  }

  void visit (HIR::WildcardPattern &pattern) override
  {
    decl_type = ctx->get_backend ()->immutable_type (decl_type);

    compiled_param
      = ctx->get_backend ()->parameter_variable (fndecl, "_", decl_type, locus);
  }

  // Empty visit for unused Pattern HIR nodes.
  void visit (HIR::GroupedPattern &) override {}
  void visit (HIR::LiteralPattern &) override {}
  void visit (HIR::PathInExpression &) override {}
  void visit (HIR::QualifiedPathInExpression &) override {}
  void visit (HIR::RangePattern &) override {}
  void visit (HIR::ReferencePattern &) override {}
  void visit (HIR::SlicePattern &) override {}
  void visit (HIR::StructPattern &) override {}
  void visit (HIR::TuplePattern &) override {}
  void visit (HIR::TupleStructPattern &) override {}

private:
  CompileFnParam (Context *ctx, tree fndecl, tree decl_type, Location locus)
    : HIRCompileBase (ctx), fndecl (fndecl), decl_type (decl_type),
      locus (locus), compiled_param (ctx->get_backend ()->error_variable ())
  {}

  tree fndecl;
  tree decl_type;
  Location locus;
  Bvariable *compiled_param;
};

class CompileSelfParam : public HIRCompileBase, public HIR::HIRStmtVisitor
{
public:
  static Bvariable *compile (Context *ctx, tree fndecl, HIR::SelfParam &self,
			     tree decl_type, Location locus)
  {
    bool is_immutable
      = self.get_self_kind () == HIR::SelfParam::ImplicitSelfKind::IMM
	|| self.get_self_kind () == HIR::SelfParam::ImplicitSelfKind::IMM_REF;
    if (is_immutable)
      decl_type = ctx->get_backend ()->immutable_type (decl_type);

    return ctx->get_backend ()->parameter_variable (fndecl, "self", decl_type,
						    locus);
  }
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_FNPARAM
