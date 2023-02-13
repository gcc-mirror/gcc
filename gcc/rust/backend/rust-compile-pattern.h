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

#include "rust-compile-base.h"

namespace Rust {
namespace Compile {

class CompilePatternCaseLabelExpr : public HIRCompileBase,
				    public HIR::HIRPatternVisitor
{
public:
  static tree Compile (HIR::Pattern *pattern, tree associated_case_label,
		       Context *ctx)
  {
    CompilePatternCaseLabelExpr compiler (ctx, associated_case_label);
    pattern->accept_vis (compiler);
    return compiler.case_label_expr;
  }

  void visit (HIR::PathInExpression &pattern) override;
  void visit (HIR::StructPattern &pattern) override;
  void visit (HIR::TupleStructPattern &pattern) override;
  void visit (HIR::WildcardPattern &pattern) override;
  void visit (HIR::RangePattern &pattern) override;

  // Empty visit for unused Pattern HIR nodes.
  void visit (HIR::IdentifierPattern &) override {}
  void visit (HIR::LiteralPattern &) override;
  void visit (HIR::QualifiedPathInExpression &) override {}
  void visit (HIR::ReferencePattern &) override {}
  void visit (HIR::SlicePattern &) override {}
  void visit (HIR::TuplePattern &) override {}

  CompilePatternCaseLabelExpr (Context *ctx, tree associated_case_label)
    : HIRCompileBase (ctx), case_label_expr (error_mark_node),
      associated_case_label (associated_case_label)
  {}

  tree case_label_expr;
  tree associated_case_label;
};

class CompilePatternBindings : public HIRCompileBase,
			       public HIR::HIRPatternVisitor
{
public:
  static void Compile (HIR::Pattern *pattern, tree match_scrutinee_expr,
		       Context *ctx)
  {
    CompilePatternBindings compiler (ctx, match_scrutinee_expr);
    pattern->accept_vis (compiler);
  }

  void visit (HIR::StructPattern &pattern) override;
  void visit (HIR::TupleStructPattern &pattern) override;

  // Empty visit for unused Pattern HIR nodes.
  void visit (HIR::IdentifierPattern &) override {}
  void visit (HIR::LiteralPattern &) override {}
  void visit (HIR::PathInExpression &) override {}
  void visit (HIR::QualifiedPathInExpression &) override {}
  void visit (HIR::RangePattern &) override {}
  void visit (HIR::ReferencePattern &) override {}
  void visit (HIR::SlicePattern &) override {}
  void visit (HIR::TuplePattern &) override {}
  void visit (HIR::WildcardPattern &) override {}

protected:
  CompilePatternBindings (Context *ctx, tree match_scrutinee_expr)
    : HIRCompileBase (ctx), match_scrutinee_expr (match_scrutinee_expr)
  {}

  tree match_scrutinee_expr;
};

class CompilePatternLet : public HIRCompileBase, public HIR::HIRPatternVisitor
{
public:
  static void Compile (HIR::Pattern *pattern, tree init_expr,
		       TyTy::BaseType *ty, Location rval_locus, Context *ctx)
  {
    CompilePatternLet compiler (ctx, init_expr, ty, rval_locus);
    pattern->accept_vis (compiler);
  }

  void visit (HIR::IdentifierPattern &) override;
  void visit (HIR::WildcardPattern &) override;
  void visit (HIR::TuplePattern &) override;

  // check for unimplemented Pattern HIR nodes.
  void visit (HIR::LiteralPattern &pattern) override
  {
    rust_sorry_at (pattern.get_locus (),
		   "literal pattern let statements not supported");
  }

  void visit (HIR::PathInExpression &pattern) override
  {
    rust_sorry_at (pattern.get_locus (),
		   "path-in-expression pattern let statements not supported");
  }

  void visit (HIR::QualifiedPathInExpression &pattern) override
  {
    rust_sorry_at (
      pattern.get_locus (),
      "qualified-path-in-expression pattern let statements not supported");
  }

  void visit (HIR::RangePattern &pattern) override
  {
    rust_sorry_at (pattern.get_locus (),
		   "range pattern let statements not supported");
  }

  void visit (HIR::ReferencePattern &pattern) override
  {
    rust_sorry_at (pattern.get_locus (),
		   "reference pattern let statements not supported");
  }

  void visit (HIR::SlicePattern &pattern) override
  {
    rust_sorry_at (pattern.get_locus (),
		   "slice pattern let statements not supported");
  }

  void visit (HIR::StructPattern &pattern) override
  {
    rust_sorry_at (pattern.get_locus (),
		   "struct pattern let statements not supported");
  }

  void visit (HIR::TupleStructPattern &pattern) override
  {
    rust_sorry_at (pattern.get_locus (),
		   "tuple-struct pattern let statements not supported");
  }

protected:
  CompilePatternLet (Context *ctx, tree init_expr, TyTy::BaseType *ty,
		     Location rval_locus)
    : HIRCompileBase (ctx), init_expr (init_expr), ty (ty),
      rval_locus (rval_locus)
  {}

  tree init_expr;
  TyTy::BaseType *ty;
  Location rval_locus;
};

} // namespace Compile
} // namespace Rust
