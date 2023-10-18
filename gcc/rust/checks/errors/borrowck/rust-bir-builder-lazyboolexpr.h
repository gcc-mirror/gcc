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

#ifndef RUST_BIR_BUILDER_LAZYBOOLEXPR_H
#define RUST_BIR_BUILDER_LAZYBOOLEXPR_H

#include "rust-bir-builder-internal.h"
#include "rust-bir-builder-expr-stmt.h"
#include "rust-hir-expr.h"

namespace Rust {
namespace BIR {

/**
 * Special builder is needed to store short-circuiting context for directly
 * nested lazy boolean expressions.
 */
class LazyBooleanExprBuilder : public AbstractBuilder,
			       public HIR::HIRExpressionVisitor
{
  BasicBlockId short_circuit_bb;

public:
  explicit LazyBooleanExprBuilder (BuilderContext &ctx)
    : AbstractBuilder (ctx), short_circuit_bb (0)
  {}

  PlaceId build (HIR::LazyBooleanExpr &expr)
  {
    PlaceId return_place = ctx.place_db.add_temporary (lookup_type (expr));

    short_circuit_bb = new_bb ();
    visit (expr);
    push_assignment (return_place, translated);
    auto final_bb = new_bb ();
    add_jump_to (final_bb);

    ctx.current_bb = short_circuit_bb;
    translated = ctx.place_db.get_constant (lookup_type (expr));
    push_assignment (return_place, translated);
    add_jump_to (final_bb);

    ctx.current_bb = final_bb;
    return return_place;
  }

protected:
  void visit (HIR::LazyBooleanExpr &expr) override
  {
    expr.get_lhs ()->accept_vis (*this);
    push_switch (translated);
    add_jump_to (short_circuit_bb);

    start_new_subsequent_bb ();
    expr.get_rhs ()->accept_vis (*this);
  }
  void visit (HIR::GroupedExpr &expr) override
  {
    expr.get_expr_in_parens ()->accept_vis (*this);
  }

protected:
public:
  void visit (HIR::QualifiedPathInExpression &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::PathInExpression &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::ClosureExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::StructExprStructFields &fields) override
  {
    ExprStmtBuilder (ctx).build (fields);
  }
  void visit (HIR::StructExprStruct &a_struct) override
  {
    ExprStmtBuilder (ctx).build (a_struct);
  }
  void visit (HIR::LiteralExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::BorrowExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::DereferenceExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::ErrorPropagationExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::NegationExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::ArithmeticOrLogicalExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::ComparisonExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::TypeCastExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::AssignmentExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::CompoundAssignmentExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::ArrayExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::ArrayIndexExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::TupleExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::TupleIndexExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::CallExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::MethodCallExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::FieldAccessExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::BlockExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::UnsafeBlockExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::LoopExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::WhileLoopExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::WhileLetLoopExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::IfExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::IfExprConseqElse &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::IfLetExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::IfLetExprConseqElse &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::MatchExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::AwaitExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }
  void visit (HIR::AsyncBlockExpr &expr) override
  {
    translated = ExprStmtBuilder (ctx).build (expr);
  }

protected: // Illegal at this position.
  void visit (HIR::StructExprFieldIdentifier &field) override
  {
    rust_unreachable ();
  }
  void visit (HIR::StructExprFieldIdentifierValue &field) override
  {
    rust_unreachable ();
  }
  void visit (HIR::StructExprFieldIndexValue &field) override
  {
    rust_unreachable ();
  }
  void visit (HIR::ContinueExpr &expr) override { rust_unreachable (); }
  void visit (HIR::BreakExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeFromToExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeFromExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeToExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeFullExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeFromToInclExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeToInclExpr &expr) override { rust_unreachable (); }
  void visit (HIR::ReturnExpr &expr) override { rust_unreachable (); }
};

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_BUILDER_LAZYBOOLEXPR_H
