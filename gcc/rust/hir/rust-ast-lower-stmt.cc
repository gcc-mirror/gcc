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

#include "optional.h"
#include "rust-ast-lower-item.h"
#include "rust-ast-lower-stmt.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower-expr.h"
#include "rust-ast-lower-pattern.h"

namespace Rust {
namespace HIR {

HIR::Stmt *
ASTLoweringStmt::translate (AST::Stmt *stmt, bool *terminated)
{
  ASTLoweringStmt resolver;
  stmt->accept_vis (resolver);

  if (!resolver.translated)
    return nullptr;

  *terminated = resolver.terminated;
  resolver.mappings.insert_location (
    resolver.translated->get_mappings ().get_hirid (),
    resolver.translated->get_locus ());
  resolver.mappings.insert_hir_stmt (resolver.translated);

  return resolver.translated;
}

void
ASTLoweringStmt::visit (AST::ExprStmt &stmt)
{
  HIR::Expr *expr = ASTLoweringExpr::translate (stmt.get_expr (), &terminated);

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, stmt.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  translated
    = new HIR::ExprStmt (mapping, std::unique_ptr<HIR::Expr> (expr),
			 stmt.get_locus (), !stmt.is_semicolon_followed ());
}

void
ASTLoweringStmt::visit (AST::ConstantItem &constant)
{
  translated = ASTLoweringItem::translate (constant);
}

void
ASTLoweringStmt::visit (AST::LetStmt &stmt)
{
  HIR::Pattern *variables
    = ASTLoweringPattern::translate (stmt.get_pattern (), true);

  tl::optional<std::unique_ptr<Type>> type = tl::nullopt;

  if (stmt.has_type ())
    type
      = std::unique_ptr<Type> (ASTLoweringType::translate (stmt.get_type ()));

  tl::optional<std::unique_ptr<HIR::Expr>> init_expr = tl::nullopt;
  tl::optional<std::unique_ptr<HIR::Expr>> else_expr = tl::nullopt;

  if (stmt.has_init_expr ())
    init_expr = std::unique_ptr<HIR::Expr> (
      ASTLoweringExpr::translate (stmt.get_init_expr ()));

  if (stmt.has_else_expr ())
    else_expr = std::unique_ptr<HIR::Expr> (
      ASTLoweringExpr::translate (stmt.get_else_expr ()));

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, stmt.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  translated
    = new HIR::LetStmt (mapping, std::unique_ptr<HIR::Pattern> (variables),
			std::move (init_expr), std::move (else_expr),
			std::move (type), stmt.get_outer_attrs (),
			stmt.get_locus ());
}

void
ASTLoweringStmt::visit (AST::TupleStruct &struct_decl)
{
  translated = ASTLoweringItem::translate (struct_decl);
}

void
ASTLoweringStmt::visit (AST::StructStruct &struct_decl)
{
  translated = ASTLoweringItem::translate (struct_decl);
}

void
ASTLoweringStmt::visit (AST::Union &union_decl)
{
  translated = ASTLoweringItem::translate (union_decl);
}

void
ASTLoweringStmt::visit (AST::Enum &enum_decl)
{
  translated = ASTLoweringItem::translate (enum_decl);
}

void
ASTLoweringStmt::visit (AST::EmptyStmt &empty)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, empty.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 mappings.get_next_localdef_id (crate_num));

  translated = new HIR::EmptyStmt (mapping, empty.get_locus ());
}

void
ASTLoweringStmt::visit (AST::Function &function)
{
  translated = ASTLoweringItem::translate (function);
}

void
ASTLoweringStmt::visit (AST::ExternBlock &extern_block)
{
  translated = lower_extern_block (extern_block);
}

void
ASTLoweringStmt::visit (AST::MacroRulesDefinition &def)
{
  lower_macro_definition (def);
}

void
ASTLoweringStmt::visit (AST::Trait &trait)
{
  translated = ASTLoweringItem::translate (trait);
}

void
ASTLoweringStmt::visit (AST::InherentImpl &impl_block)
{
  translated = ASTLoweringItem::translate (impl_block);
}

void
ASTLoweringStmt::visit (AST::TraitImpl &impl_block)
{
  translated = ASTLoweringItem::translate (impl_block);
}

void
ASTLoweringStmt::visit (AST::StaticItem &var)
{
  translated = ASTLoweringItem::translate (var);
}

} // namespace HIR
} // namespace Rust
