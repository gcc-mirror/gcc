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

#ifndef RUST_COMPILE_VAR_DECL
#define RUST_COMPILE_VAR_DECL

#include "rust-compile-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Compile {

class CompileVarDecl : public HIRCompileBase,
		       public HIR::HIRPatternVisitor,
		       public HIR::HIRStmtVisitor
{
  using HIR::HIRPatternVisitor::visit;
  using HIR::HIRStmtVisitor::visit;

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

    translated_type = TyTyResolveCompile::compile (ctx, resolved_type);
    stmt.get_pattern ()->accept_vis (
      static_cast<HIR::HIRPatternVisitor &> (*this));
  }

  void visit (HIR::IdentifierPattern &pattern) override
  {
    if (!pattern.is_mut ())
      translated_type = ctx->get_backend ()->immutable_type (translated_type);

    compiled_variable
      = ctx->get_backend ()->local_variable (fndecl, pattern.get_identifier (),
					     translated_type, NULL /*decl_var*/,
					     locus);
  }

  void visit (HIR::WildcardPattern &pattern) override
  {
    translated_type = ctx->get_backend ()->immutable_type (translated_type);

    compiled_variable
      = ctx->get_backend ()->local_variable (fndecl, "_", translated_type,
					     NULL /*decl_var*/, locus);
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

  // Empty visit for unused Stmt HIR nodes.
  void visit (HIR::EnumItemTuple &) override {}
  void visit (HIR::EnumItemStruct &) override {}
  void visit (HIR::EnumItem &item) override {}
  void visit (HIR::TupleStruct &tuple_struct) override {}
  void visit (HIR::EnumItemDiscriminant &) override {}
  void visit (HIR::TypePathSegmentFunction &segment) override {}
  void visit (HIR::TypePath &path) override {}
  void visit (HIR::QualifiedPathInType &path) override {}
  void visit (HIR::Module &module) override {}
  void visit (HIR::ExternCrate &crate) override {}
  void visit (HIR::UseDeclaration &use_decl) override {}
  void visit (HIR::Function &function) override {}
  void visit (HIR::TypeAlias &type_alias) override {}
  void visit (HIR::StructStruct &struct_item) override {}
  void visit (HIR::Enum &enum_item) override {}
  void visit (HIR::Union &union_item) override {}
  void visit (HIR::ConstantItem &const_item) override {}
  void visit (HIR::StaticItem &static_item) override {}
  void visit (HIR::Trait &trait) override {}
  void visit (HIR::ImplBlock &impl) override {}
  void visit (HIR::ExternBlock &block) override {}
  void visit (HIR::EmptyStmt &stmt) override {}
  void visit (HIR::ExprStmtWithoutBlock &stmt) override {}
  void visit (HIR::ExprStmtWithBlock &stmt) override {}

private:
  CompileVarDecl (Context *ctx, tree fndecl)
    : HIRCompileBase (ctx), fndecl (fndecl), translated_type (error_mark_node),
      compiled_variable (ctx->get_backend ()->error_variable ())
  {}

  tree fndecl;
  tree translated_type;
  Location locus;
  Bvariable *compiled_variable;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_VAR_DECL
