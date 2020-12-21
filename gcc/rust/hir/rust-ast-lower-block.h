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
  static HIR::BlockExpr *translate (AST::BlockExpr *expr)
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

    return resolver.translated;
  }

  ~ASTLoweringBlock () {}

  void visit (AST::BlockExpr &expr);

private:
  ASTLoweringBlock () : ASTLoweringBase (), translated (nullptr) {}

  HIR::BlockExpr *translated;
};

class ASTLoweringIfBlock : public ASTLoweringBase
{
public:
  static HIR::IfExpr *translate (AST::IfExpr *expr)
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

    return resolver.translated;
  }

  ~ASTLoweringIfBlock () {}

  void visit (AST::IfExpr &expr);

  void visit (AST::IfExprConseqElse &expr);

  void visit (AST::IfExprConseqIf &expr);

private:
  ASTLoweringIfBlock () : ASTLoweringBase (), translated (nullptr) {}

  HIR::IfExpr *translated;
};

class ASTLoweringExprWithBlock : public ASTLoweringBase
{
public:
  static HIR::ExprWithBlock *translate (AST::ExprWithBlock *expr)
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

    return resolver.translated;
  }

  ~ASTLoweringExprWithBlock () {}

  void visit (AST::IfExpr &expr)
  {
    translated = ASTLoweringIfBlock::translate (&expr);
  }

  void visit (AST::IfExprConseqElse &expr)
  {
    translated = ASTLoweringIfBlock::translate (&expr);
  }

  void visit (AST::IfExprConseqIf &expr)
  {
    translated = ASTLoweringIfBlock::translate (&expr);
  }

private:
  ASTLoweringExprWithBlock () : ASTLoweringBase (), translated (nullptr) {}

  HIR::ExprWithBlock *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_BLOCK
