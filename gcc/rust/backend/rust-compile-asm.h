
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

#ifndef RUST_COMPILE_ASM
#define RUST_COMPILE_ASM

#include "rust-compile-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Compile {

class CompileAsm : private HIRCompileBase, protected HIR::HIRExpressionVisitor
{
public:
  // static tree Compile (HIR::Expr *expr, Context *ctx);

  void visit (HIR::InlineAsm &expr) override;

  void visit (HIR::TupleIndexExpr &expr) override {}
  void visit (HIR::TupleExpr &expr) override {}
  void visit (HIR::ReturnExpr &expr) override {}
  void visit (HIR::CallExpr &expr) override {}
  void visit (HIR::MethodCallExpr &expr) override {}
  void visit (HIR::LiteralExpr &expr) override {}
  void visit (HIR::AssignmentExpr &expr) override {}
  void visit (HIR::CompoundAssignmentExpr &expr) override {}
  void visit (HIR::ArrayIndexExpr &expr) override {}
  void visit (HIR::ArrayExpr &expr) override {}
  void visit (HIR::ArithmeticOrLogicalExpr &expr) override {}
  void visit (HIR::ComparisonExpr &expr) override {}
  void visit (HIR::LazyBooleanExpr &expr) override {}
  void visit (HIR::NegationExpr &expr) override {}
  void visit (HIR::TypeCastExpr &expr) override {}
  void visit (HIR::IfExpr &expr) override {}
  void visit (HIR::IfExprConseqElse &expr) override {}
  void visit (HIR::BlockExpr &expr) override {}
  void visit (HIR::UnsafeBlockExpr &expr) override {}
  void visit (HIR::StructExprStruct &struct_expr) override {}
  void visit (HIR::StructExprStructFields &struct_expr) override {}
  void visit (HIR::GroupedExpr &expr) override {}
  void visit (HIR::FieldAccessExpr &expr) override {}
  void visit (HIR::QualifiedPathInExpression &expr) override {}
  void visit (HIR::PathInExpression &expr) override {}
  void visit (HIR::LoopExpr &expr) override {}
  void visit (HIR::WhileLoopExpr &expr) override {}
  void visit (HIR::BreakExpr &expr) override {}
  void visit (HIR::ContinueExpr &expr) override {}
  void visit (HIR::BorrowExpr &expr) override {}
  void visit (HIR::DereferenceExpr &expr) override {}
  void visit (HIR::MatchExpr &expr) override {}
  void visit (HIR::RangeFromToExpr &expr) override {}
  void visit (HIR::RangeFromExpr &expr) override {}
  void visit (HIR::RangeToExpr &expr) override {}
  void visit (HIR::RangeFullExpr &expr) override {}
  void visit (HIR::RangeFromToInclExpr &expr) override {}
  void visit (HIR::ClosureExpr &expr) override {}

  // TODO
  void visit (HIR::ErrorPropagationExpr &) override {}
  void visit (HIR::RangeToInclExpr &) override {}

  // TODO
  // these need to be sugared in the HIR to if statements and a match
  void visit (HIR::WhileLetLoopExpr &) override {}
  void visit (HIR::IfLetExpr &) override {}
  void visit (HIR::IfLetExprConseqElse &) override {}

  // lets not worry about async yet....
  void visit (HIR::AwaitExpr &) override {}
  void visit (HIR::AsyncBlockExpr &) override {}

  // nothing to do for these
  void visit (HIR::StructExprFieldIdentifier &) override {}
  void visit (HIR::StructExprFieldIdentifierValue &) override {}
  void visit (HIR::StructExprFieldIndexValue &) override {}

  static const int ASM_TREE_ARRAY_LENGTH = 5;
  static tree asm_build_asm_stmt (HIR::InlineAsm &);
  static tree asm_build_expr (HIR::InlineAsm &);
  static tree asm_build_stmt (location_t, enum tree_code,
			      const std::array<tree, ASM_TREE_ARRAY_LENGTH> &);

  static location_t asm_get_locus (HIR::InlineAsm &);
  static tree asm_construct_string_tree (HIR::InlineAsm &);
  static tree asm_construct_outputs (HIR::InlineAsm &);
  static tree asm_construct_inputs (HIR::InlineAsm &);
  static tree asm_construct_clobber_tree (HIR::InlineAsm &);
  static tree asm_construct_label_tree (HIR::InlineAsm &);
  static bool asm_is_simple (HIR::InlineAsm &);
  static bool asm_is_inline (HIR::InlineAsm &);

  CompileAsm (Context *ctx);

private:
  tree translated;
};
} // namespace Compile
} // namespace Rust
#endif // RUST_COMPILE_ASM
