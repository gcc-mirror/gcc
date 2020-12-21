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

#include "rust-ast-lower.h"
#include "rust-ast-lower-item.h"
#include "rust-ast-lower-expr.h"
#include "rust-ast-lower-block.h"

namespace Rust {
namespace HIR {

ASTLowering::ASTLowering (AST::Crate &astCrate) : astCrate (astCrate) {}

ASTLowering::~ASTLowering () {}

HIR::Crate
ASTLowering::Resolve (AST::Crate &astCrate)
{
  ASTLowering resolver (astCrate);
  return resolver.go ();
}

HIR::Crate
ASTLowering::go ()
{
  std::vector<std::unique_ptr<HIR::Item> > items;
  std::vector<HIR::Attribute> inner_attrs;
  bool has_utf8bom = false;
  bool has_shebang = false;

  for (auto it = astCrate.items.begin (); it != astCrate.items.end (); it++)
    {
      auto translated = ASTLoweringItem::translate (it->get ());
      if (translated != nullptr)
	items.push_back (std::unique_ptr<HIR::Item> (translated));
    }

  auto mappings = Analysis::Mappings::get ();
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, UNKNOWN_NODEID,
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  return HIR::Crate (std::move (items), std::move (inner_attrs), mapping,
		     has_utf8bom, has_shebang);
}

// rust-ast-lower-block.h
void
ASTLoweringBlock::visit (AST::BlockExpr &expr)
{
  std::vector<std::unique_ptr<HIR::Stmt> > block_stmts;
  std::unique_ptr<HIR::ExprWithoutBlock> block_expr;
  std::vector<HIR::Attribute> inner_attribs;
  std::vector<HIR::Attribute> outer_attribs;

  expr.iterate_stmts ([&] (AST::Stmt *s) mutable -> bool {
    auto translated_stmt = ASTLoweringStmt::translate (s);
    block_stmts.push_back (std::unique_ptr<HIR::Stmt> (translated_stmt));
    return true;
  });

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::BlockExpr (mapping, std::move (block_stmts),
			  std::move (block_expr), std::move (inner_attribs),
			  std::move (outer_attribs), expr.get_locus ());
}

void
ASTLoweringIfBlock::visit (AST::IfExpr &expr)
{
  HIR::Expr *condition
    = ASTLoweringExpr::translate (expr.get_condition_expr ().get ());
  HIR::BlockExpr *block
    = ASTLoweringBlock::translate (expr.get_if_block ().get ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::IfExpr (mapping, std::unique_ptr<HIR::Expr> (condition),
				std::unique_ptr<HIR::BlockExpr> (block),
				expr.get_locus ());
}

void
ASTLoweringIfBlock::visit (AST::IfExprConseqElse &expr)
{
  HIR::Expr *condition
    = ASTLoweringExpr::translate (expr.get_condition_expr ().get ());
  HIR::BlockExpr *if_block
    = ASTLoweringBlock::translate (expr.get_if_block ().get ());
  HIR::BlockExpr *else_block
    = ASTLoweringBlock::translate (expr.get_else_block ().get ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::IfExprConseqElse (mapping,
				 std::unique_ptr<HIR::Expr> (condition),
				 std::unique_ptr<HIR::BlockExpr> (if_block),
				 std::unique_ptr<HIR::BlockExpr> (else_block),
				 expr.get_locus ());
}

void
ASTLoweringIfBlock::visit (AST::IfExprConseqIf &expr)
{
  HIR::Expr *condition
    = ASTLoweringExpr::translate (expr.get_condition_expr ().get ());
  HIR::BlockExpr *block
    = ASTLoweringBlock::translate (expr.get_if_block ().get ());
  HIR::IfExpr *conseq_if_expr
    = ASTLoweringIfBlock::translate (expr.get_conseq_if_expr ().get ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::IfExprConseqIf (mapping, std::unique_ptr<HIR::Expr> (condition),
			       std::unique_ptr<HIR::BlockExpr> (block),
			       std::unique_ptr<HIR::IfExpr> (conseq_if_expr),
			       expr.get_locus ());
}

} // namespace HIR
} // namespace Rust
