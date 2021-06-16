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

  void visit (HIR::BlockExpr &expr) override
  {
    expr.iterate_stmts ([&] (HIR::Stmt *s) mutable -> bool {
      s->accept_vis (*this);
      return true;
    });
    if (expr.has_expr ())
      {
	expr.get_final_expr ().get ()->accept_vis (*this);
      }
  }
  void visit (HIR::Function &function) override
  {
    function.get_definition ().get ()->accept_vis (*this);
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

  void visit (HIR::Method &method) override
  {
    method.get_definition ().get ()->accept_vis (*this);
  }

  void visit (HIR::TraitItemFunc &item) override
  {
    item.get_block_expr ()->accept_vis (*this);
  }

  void visit (HIR::TraitItemMethod &item) override
  {
    item.get_block_expr ()->accept_vis (*this);
  }

  void visit (HIR::ImplBlock &impl) override
  {
    for (auto &&item : impl.get_impl_items ())
      {
	item.get ()->accept_vis (*this);
      }
  }

  void visit (HIR::LetStmt &stmt) override
  {
    if (stmt.has_init_expr ())
      {
	stmt.get_init_expr ()->accept_vis (*this);
      }
  }

  void visit (HIR::StructExprStructFields &stct) override
  {
    stct.iterate ([&] (HIR::StructExprField *field) -> bool {
      field->accept_vis (*this);
      return true;
    });
    if (stct.has_struct_base ())
      {
	stct.struct_base->base_struct.get ()->accept_vis (*this);
      }
  }

  void visit (HIR::StructExprStructBase &stct) override
  {
    stct.get_struct_base ()->base_struct.get ()->accept_vis (*this);
  }

private:
  std::vector<HirId> worklist;
  std::set<HirId> liveSymbols;
  std::set<HirId> scannedSymbols;
  Analysis::Mappings *mappings;
  Resolver::Resolver *resolver;
  MarkLive (std::vector<HirId> worklist)
    : worklist (worklist), mappings (Analysis::Mappings::get ()),
      resolver (Resolver::Resolver::get ()){};
};

} // namespace Analysis
} // namespace Rust

#endif
