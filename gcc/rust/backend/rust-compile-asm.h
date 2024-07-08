
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

std::string
strip_double_quotes (const std::string &);

class CompileAsm : private HIRCompileBase, protected HIR::HIRExpressionVisitor
{
private:
  tree translated;

public:
  // WE WILL OPEN THIS UP WHEN WE WANT TO ADD A DEDICATED PASS OF HIR'S ASM
  // translation.
  // static tree Compile (HIR::Expr *expr, Context *ctx);

  // RELEVANT MEMBER FUNCTIONS
  static const int ASM_TREE_ARRAY_LENGTH = 5;
  static tree asm_build_expr (HIR::InlineAsm &);
  static tree asm_build_stmt (location_t,
			      const std::array<tree, ASM_TREE_ARRAY_LENGTH> &);

  static tree asm_construct_string_tree (HIR::InlineAsm &);
  static tree asm_construct_outputs (HIR::InlineAsm &);
  static tree asm_construct_inputs (HIR::InlineAsm &);
  static tree asm_construct_clobber_tree (HIR::InlineAsm &);
  static tree asm_construct_label_tree (HIR::InlineAsm &);
  static bool asm_is_simple (HIR::InlineAsm &);
  static bool asm_is_inline (HIR::InlineAsm &);

  CompileAsm (Context *ctx);

  void visit (HIR::InlineAsm &) override;

  // NON RELEVANT MEMBER FUNCTIONS

  void visit (HIR::TupleIndexExpr &) override {}
  void visit (HIR::TupleExpr &) override {}
  void visit (HIR::ReturnExpr &) override {}
  void visit (HIR::CallExpr &) override {}
  void visit (HIR::MethodCallExpr &) override {}
  void visit (HIR::LiteralExpr &) override {}
  void visit (HIR::AssignmentExpr &) override {}
  void visit (HIR::CompoundAssignmentExpr &) override {}
  void visit (HIR::ArrayIndexExpr &) override {}
  void visit (HIR::ArrayExpr &) override {}
  void visit (HIR::ArithmeticOrLogicalExpr &) override {}
  void visit (HIR::ComparisonExpr &) override {}
  void visit (HIR::LazyBooleanExpr &) override {}
  void visit (HIR::NegationExpr &) override {}
  void visit (HIR::TypeCastExpr &) override {}
  void visit (HIR::IfExpr &) override {}
  void visit (HIR::IfExprConseqElse &) override {}
  void visit (HIR::BlockExpr &) override {}
  void visit (HIR::UnsafeBlockExpr &) override {}
  void visit (HIR::StructExprStruct &struct_) override {}
  void visit (HIR::StructExprStructFields &struct_) override {}
  void visit (HIR::GroupedExpr &) override {}
  void visit (HIR::FieldAccessExpr &) override {}
  void visit (HIR::QualifiedPathInExpression &) override {}
  void visit (HIR::PathInExpression &) override {}
  void visit (HIR::LoopExpr &) override {}
  void visit (HIR::WhileLoopExpr &) override {}
  void visit (HIR::BreakExpr &) override {}
  void visit (HIR::ContinueExpr &) override {}
  void visit (HIR::BorrowExpr &) override {}
  void visit (HIR::DereferenceExpr &) override {}
  void visit (HIR::MatchExpr &) override {}
  void visit (HIR::RangeFromToExpr &) override {}
  void visit (HIR::RangeFromExpr &) override {}
  void visit (HIR::RangeToExpr &) override {}
  void visit (HIR::RangeFullExpr &) override {}
  void visit (HIR::RangeFromToInclExpr &) override {}
  void visit (HIR::ClosureExpr &) override {}
  void visit (HIR::ErrorPropagationExpr &) override {}
  void visit (HIR::RangeToInclExpr &) override {}
  void visit (HIR::WhileLetLoopExpr &) override {}
  void visit (HIR::IfLetExpr &) override {}
  void visit (HIR::IfLetExprConseqElse &) override {}
  void visit (HIR::AwaitExpr &) override {}
  void visit (HIR::AsyncBlockExpr &) override {}
  void visit (HIR::StructExprFieldIdentifier &) override {}
  void visit (HIR::StructExprFieldIdentifierValue &) override {}
  void visit (HIR::StructExprFieldIndexValue &) override {}
};
} // namespace Compile
} // namespace Rust
#endif // RUST_COMPILE_ASM
