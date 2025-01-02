// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#ifndef RUST_AST_LOWER_BLOCK
#define RUST_AST_LOWER_BLOCK

#include "rust-diagnostics.h"
#include "rust-ast-lower-base.h"

namespace Rust {
namespace HIR {

class ASTLoweringBlock : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::BlockExpr *translate (AST::BlockExpr &expr, bool *terminated)
  {
    ASTLoweringBlock resolver;
    expr.normalize_tail_expr ();
    expr.accept_vis (resolver);
    if (resolver.translated != nullptr)
      {
	resolver.mappings->insert_hir_expr (resolver.translated);
      }

    *terminated = resolver.terminated;
    return resolver.translated;
  }

  static HIR::UnsafeBlockExpr *translate (AST::UnsafeBlockExpr &expr,
					  bool *terminated)
  {
    ASTLoweringBlock resolver;

    HIR::BlockExpr *block
      = ASTLoweringBlock::translate (expr.get_block_expr (), terminated);
    auto crate_num = resolver.mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				   resolver.mappings->get_next_hir_id (
				     crate_num),
				   UNKNOWN_LOCAL_DEFID);

    HIR::UnsafeBlockExpr *translated
      = new HIR::UnsafeBlockExpr (mapping,
				  std::unique_ptr<HIR::BlockExpr> (block),
				  expr.get_outer_attrs (), expr.get_locus ());

    resolver.mappings->insert_hir_expr (translated);

    return translated;
  }

  void visit (AST::BlockExpr &expr) override;

private:
  ASTLoweringBlock ()
    : ASTLoweringBase (), translated (nullptr), terminated (false)
  {}

  HIR::BlockExpr *translated;
  bool terminated;
};

class ASTLoweringIfBlock : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::IfExpr *translate (AST::IfExpr &expr, bool *terminated)
  {
    ASTLoweringIfBlock resolver;
    expr.accept_vis (resolver);
    if (resolver.translated != nullptr)
      {
	resolver.mappings->insert_hir_expr (resolver.translated);
      }
    *terminated = resolver.terminated;
    return resolver.translated;
  }

  ~ASTLoweringIfBlock () {}

  void visit (AST::IfExpr &expr) override;

  void visit (AST::IfExprConseqElse &expr) override;

private:
  ASTLoweringIfBlock ()
    : ASTLoweringBase (), translated (nullptr), terminated (false)
  {}

  HIR::IfExpr *translated;
  bool terminated;
};

class ASTLoweringIfLetBlock : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::IfLetExpr *translate (AST::IfLetExpr &expr)
  {
    ASTLoweringIfLetBlock resolver;
    expr.accept_vis (resolver);
    if (resolver.translated != nullptr)
      {
	resolver.mappings->insert_hir_expr (resolver.translated);
      }
    return resolver.translated;
  }

  ~ASTLoweringIfLetBlock () {}

  void visit (AST::IfLetExpr &expr) override;

  void visit (AST::IfLetExprConseqElse &expr) override;

private:
  ASTLoweringIfLetBlock () : ASTLoweringBase (), translated (nullptr) {}

  HIR::IfLetExpr *translated;
};

class ASTLoweringExprWithBlock : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::ExprWithBlock *translate (AST::ExprWithBlock &expr,
					bool *terminated)
  {
    ASTLoweringExprWithBlock resolver;
    expr.accept_vis (resolver);
    if (resolver.translated != nullptr)
      {
	resolver.mappings->insert_hir_expr (resolver.translated);
      }

    *terminated = resolver.terminated;
    return resolver.translated;
  }

  ~ASTLoweringExprWithBlock () {}

  void visit (AST::IfExpr &expr) override
  {
    translated = ASTLoweringIfBlock::translate (expr, &terminated);
  }

  void visit (AST::IfExprConseqElse &expr) override
  {
    translated = ASTLoweringIfBlock::translate (expr, &terminated);
  }

  void visit (AST::IfLetExpr &expr) override
  {
    translated = ASTLoweringIfLetBlock::translate (expr);
  }

  void visit (AST::IfLetExprConseqElse &expr) override
  {
    translated = ASTLoweringIfLetBlock::translate (expr);
  }

  void visit (AST::BlockExpr &expr) override
  {
    translated = ASTLoweringBlock::translate (expr, &terminated);
  }

  void visit (AST::UnsafeBlockExpr &expr) override
  {
    translated = ASTLoweringBlock::translate (expr, &terminated);
  }

  void visit (AST::LoopExpr &expr) override
  {
    HIR::BlockExpr *loop_block
      = ASTLoweringBlock::translate (expr.get_loop_block (), &terminated);

    HIR::LoopLabel loop_label = lower_loop_label (expr.get_loop_label ());

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated
      = new HIR::LoopExpr (mapping,
			   std::unique_ptr<HIR::BlockExpr> (loop_block),
			   expr.get_locus (), std::move (loop_label),
			   expr.get_outer_attrs ());
  }

  void visit (AST::WhileLoopExpr &expr) override;

  void visit (AST::ForLoopExpr &expr) override;

  void visit (AST::MatchExpr &expr) override;

private:
  ASTLoweringExprWithBlock ()
    : ASTLoweringBase (), translated (nullptr), terminated (false)
  {}

  HIR::ExprWithBlock *translated;
  bool terminated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_BLOCK
