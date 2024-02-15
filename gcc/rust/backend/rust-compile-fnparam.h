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

#ifndef RUST_COMPILE_FNPARAM
#define RUST_COMPILE_FNPARAM

#include "rust-compile-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Compile {

class CompileFnParam : private HIRCompileBase, protected HIR::HIRPatternVisitor
{
public:
  static Bvariable *compile (Context *ctx, tree fndecl,
			     HIR::FunctionParam *param, tree decl_type,
			     location_t locus);
  static Bvariable *compile (Context *ctx, tree fndecl, HIR::Pattern *param,
			     tree decl_type, location_t locus);

  void visit (HIR::IdentifierPattern &pattern) override;
  void visit (HIR::WildcardPattern &pattern) override;
  void visit (HIR::StructPattern &) override;
  void visit (HIR::TupleStructPattern &) override;
  void visit (HIR::ReferencePattern &) override;

  // Empty visit for unused Pattern HIR nodes.
  void visit (HIR::AltPattern &) override {}
  void visit (HIR::LiteralPattern &) override {}
  void visit (HIR::PathInExpression &) override {}
  void visit (HIR::QualifiedPathInExpression &) override {}
  void visit (HIR::RangePattern &) override {}
  void visit (HIR::SlicePattern &) override {}
  void visit (HIR::TuplePattern &) override {}

private:
  CompileFnParam (Context *ctx, tree fndecl, tree decl_type, location_t locus);

  tree create_tmp_param_var (tree decl_type);

  tree fndecl;
  tree decl_type;
  location_t locus;
  Bvariable *compiled_param;
};

class CompileSelfParam : private HIRCompileBase
{
public:
  static Bvariable *compile (Context *ctx, tree fndecl, HIR::SelfParam &self,
			     tree decl_type, location_t locus);
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_FNPARAM
