// Copyright (C) 2020 Free Software Foundation, Inc.

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
public:
  static HIR::BlockExpr *translate (AST::BlockExpr *expr, bool *terminated)
  {
    ASTLoweringBlock resolver;
    expr->accept_vis (resolver);
    if (resolver.translated != nullptr)
      {
	resolver.mappings->insert_hir_expr (
	  resolver.translated->get_mappings ().get_crate_num (),
	  resolver.translated->get_mappings ().get_hirid (),
	  resolver.translated);
      }

    *terminated = resolver.terminated;
    return resolver.translated;
  }

  ~ASTLoweringBlock () {}

  void visit (AST::BlockExpr &expr);

private:
  ASTLoweringBlock ()
    : ASTLoweringBase (), translated (nullptr), terminated (false)
  {}

  HIR::BlockExpr *translated;
  bool terminated;
};

class ASTLoweringIfBlock : public ASTLoweringBase
{
public:
  static HIR::IfExpr *translate (AST::IfExpr *expr, bool *terminated)
  {
    ASTLoweringIfBlock resolver;
    expr->accept_vis (resolver);
    if (resolver.translated != nullptr)
      {
	resolver.mappings->insert_hir_expr (
	  resolver.translated->get_mappings ().get_crate_num (),
	  resolver.translated->get_mappings ().get_hirid (),
	  resolver.translated);
      }
    *terminated = resolver.terminated;
    return resolver.translated;
  }

  ~ASTLoweringIfBlock () {}

  void visit (AST::IfExpr &expr);

  void visit (AST::IfExprConseqElse &expr);

  void visit (AST::IfExprConseqIf &expr);

private:
  ASTLoweringIfBlock ()
    : ASTLoweringBase (), translated (nullptr), terminated (false)
  {}

  HIR::IfExpr *translated;
  bool terminated;
};

class ASTLoweringExprWithBlock : public ASTLoweringBase
{
public:
  static HIR::ExprWithBlock *translate (AST::ExprWithBlock *expr,
					bool *terminated)
  {
    ASTLoweringExprWithBlock resolver;
    expr->accept_vis (resolver);
    if (resolver.translated != nullptr)
      {
	resolver.mappings->insert_hir_expr (
	  resolver.translated->get_mappings ().get_crate_num (),
	  resolver.translated->get_mappings ().get_hirid (),
	  resolver.translated);
      }

    *terminated = resolver.terminated;
    return resolver.translated;
  }

  ~ASTLoweringExprWithBlock () {}

  void visit (AST::IfExpr &expr)
  {
    translated = ASTLoweringIfBlock::translate (&expr, &terminated);
  }

  void visit (AST::IfExprConseqElse &expr)
  {
    translated = ASTLoweringIfBlock::translate (&expr, &terminated);
  }

  void visit (AST::IfExprConseqIf &expr)
  {
    translated = ASTLoweringIfBlock::translate (&expr, &terminated);
  }

  void visit (AST::BlockExpr &expr)
  {
    translated = ASTLoweringBlock::translate (&expr, &terminated);
  }

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
