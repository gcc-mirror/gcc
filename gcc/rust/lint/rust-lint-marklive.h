// Copyright (C) 2021 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_LIVENESS
#define RUST_HIR_LIVENESS

#include <set>
#include "rust-hir-full-decls.h"
#include "rust-hir-map.h"
#include "rust-lint-marklive-base.h"
#include "rust-name-resolver.h"

namespace Rust {
namespace Analysis {

class MarkLive : public MarkLiveBase
{
  using Rust::Analysis::MarkLiveBase::visit;

public:
  static std::set<HirId> Analysis (HIR::Crate &crate);
  void go (HIR::Crate &crate);

  void visit (HIR::PathInExpression &expr) override;
  void visit (HIR::IdentifierExpr &expr) override;
  void visit (HIR::FieldAccessExpr &expr) override;
  void visit (HIR::TupleIndexExpr &expr) override;
  void visit (HIR::MethodCallExpr &expr) override;
  void visit (HIR::TypeAlias &alias) override;

  void visit (HIR::BorrowExpr &expr) override
  {
    expr.get_expr ()->accept_vis (*this);
  }

  void visit (HIR::DereferenceExpr &expr) override
  {
    expr.get_expr ()->accept_vis (*this);
  }

  void visit (HIR::NegationExpr &expr) override
  {
    expr.get_expr ()->accept_vis (*this);
  }

  void visit (HIR::LazyBooleanExpr &expr) override
  {
    expr.get_lhs ()->accept_vis (*this);
    expr.get_rhs ()->accept_vis (*this);
  }

  void visit (HIR::TypeCastExpr &expr) override
  {
    expr.get_expr ()->accept_vis (*this);
  }

  void visit (HIR::GroupedExpr &expr) override
  {
    expr.get_expr_in_parens ()->accept_vis (*this);
  }

  void visit (HIR::ArrayExpr &expr) override
  {
    expr.get_internal_elements ()->accept_vis (*this);
  }

  void visit (HIR::ArrayElemsValues &expr) override
  {
    expr.iterate ([&] (HIR::Expr *expr) mutable -> bool {
      expr->accept_vis (*this);
      return true;
    });
  }

  void visit (HIR::TupleExpr &expr) override
  {
    expr.iterate ([&] (HIR::Expr *expr) mutable -> bool {
      expr->accept_vis (*this);
      return true;
    });
  }

  void visit (HIR::BlockExpr &expr) override
  {
    expr.iterate_stmts ([&] (HIR::Stmt *s) mutable -> bool {
      s->accept_vis (*this);
      return true;
    });
    if (expr.has_expr ())
      {
	expr.get_final_expr ()->accept_vis (*this);
      }
  }

  void visit (HIR::LoopExpr &expr) override
  {
    expr.get_loop_block ()->accept_vis (*this);
  }

  void visit (HIR::BreakExpr &expr) override
  {
    if (expr.has_break_expr ())
      expr.get_expr ()->accept_vis (*this);
  }

  void visit (HIR::WhileLoopExpr &expr) override
  {
    expr.get_loop_block ()->accept_vis (*this);
    expr.get_predicate_expr ()->accept_vis (*this);
  }

  void visit (HIR::Function &function) override
  {
    function.get_definition ()->accept_vis (*this);
  }

  void visit (HIR::ReturnExpr &expr) override
  {
    if (expr.has_return_expr ())
      expr.get_expr ()->accept_vis (*this);
  }

  void visit (HIR::WhileLetLoopExpr &expr) override
  {
    expr.get_loop_block ()->accept_vis (*this);
    expr.get_cond ()->accept_vis (*this);
  }

  void visit (HIR::ForLoopExpr &expr) override
  {
    expr.get_loop_block ()->accept_vis (*this);
    expr.get_iterator_expr ()->accept_vis (*this);
  }

  void visit (HIR::ExprStmtWithoutBlock &stmt) override
  {
    stmt.get_expr ()->accept_vis (*this);
  }

  void visit (HIR::ExprStmtWithBlock &stmt) override
  {
    stmt.get_expr ()->accept_vis (*this);
  }

  void visit (HIR::CallExpr &expr) override
  {
    expr.get_fnexpr ()->accept_vis (*this);
    expr.iterate_params ([&] (HIR::Expr *expr) -> bool {
      expr->accept_vis (*this);
      return true;
    });
  }

  void visit (HIR::ArithmeticOrLogicalExpr &expr) override
  {
    expr.visit_lhs (*this);
    expr.visit_rhs (*this);
  }
  void visit (HIR::ComparisonExpr &expr) override
  {
    expr.get_lhs ()->accept_vis (*this);
    expr.get_rhs ()->accept_vis (*this);
  }

  void visit (HIR::AssignmentExpr &expr) override
  {
    expr.visit_lhs (*this);
    expr.visit_rhs (*this);
  }

  void visit (HIR::IfExpr &expr) override
  {
    expr.get_if_condition ()->accept_vis (*this);
    expr.get_if_block ()->accept_vis (*this);
  }

  void visit (HIR::IfExprConseqElse &expr) override
  {
    expr.get_if_condition ()->accept_vis (*this);
    expr.get_if_block ()->accept_vis (*this);
    expr.get_else_block ()->accept_vis (*this);
  }

  void visit (HIR::IfExprConseqIf &expr) override
  {
    expr.get_if_condition ()->accept_vis (*this);
    expr.get_if_block ()->accept_vis (*this);
    expr.get_conseq_if_expr ()->accept_vis (*this);
  }

  void visit (HIR::TraitItemFunc &item) override
  {
    item.get_block_expr ()->accept_vis (*this);
  }

  void visit (HIR::ImplBlock &impl) override
  {
    for (auto &&item : impl.get_impl_items ())
      {
	item->accept_vis (*this);
      }
  }

  void visit (HIR::LetStmt &stmt) override
  {
    if (stmt.has_init_expr ())
      {
	stmt.get_init_expr ()->accept_vis (*this);
      }
  }

  void visit (HIR::StructExprStruct &stct) override
  {
    stct.get_struct_name ().accept_vis (*this);
  }

  void visit (HIR::StructExprStructFields &stct) override
  {
    stct.iterate ([&] (HIR::StructExprField *field) -> bool {
      field->accept_vis (*this);
      return true;
    });

    stct.get_struct_name ().accept_vis (*this);
    if (stct.has_struct_base ())
      {
	stct.struct_base->base_struct->accept_vis (*this);
      }
  }

  virtual void visit (HIR::StructExprFieldIdentifierValue &field) override
  {
    field.get_value ()->accept_vis (*this);
  }

  void visit (HIR::StructExprStructBase &stct) override
  {
    stct.get_struct_base ()->base_struct->accept_vis (*this);
  }

private:
  std::vector<HirId> worklist;
  std::set<HirId> liveSymbols;
  std::set<HirId> scannedSymbols;
  Analysis::Mappings *mappings;
  Resolver::Resolver *resolver;
  Resolver::TypeCheckContext *tyctx;
  MarkLive (std::vector<HirId> worklist)
    : worklist (worklist), mappings (Analysis::Mappings::get ()),
      resolver (Resolver::Resolver::get ()),
      tyctx (Resolver::TypeCheckContext::get ()){};

  void mark_hir_id (HirId);
  bool visit_path_segment (HIR::PathExprSegment);
  void find_ref_node_id (NodeId ast_node_id, NodeId &ref_node_id);
};

} // namespace Analysis
} // namespace Rust

#endif
