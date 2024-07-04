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
  resolver.mappings->insert_location (
    resolver.translated->get_mappings ().get_hirid (),
    resolver.translated->get_locus ());
  resolver.mappings->insert_hir_stmt (resolver.translated);

  return resolver.translated;
}

void
ASTLoweringStmt::visit (AST::ExprStmt &stmt)
{
  HIR::Expr *expr
    = ASTLoweringExpr::translate (stmt.get_expr ().get (), &terminated);

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, stmt.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  translated
    = new HIR::ExprStmt (mapping, std::unique_ptr<HIR::Expr> (expr),
			 stmt.get_locus (), !stmt.is_semicolon_followed ());
}

void
ASTLoweringStmt::visit (AST::ConstantItem &constant)
{
  translated = ASTLoweringItem::translate (&constant);
}

void
ASTLoweringStmt::visit (AST::LetStmt &stmt)
{
  HIR::Pattern *variables
    = ASTLoweringPattern::translate (stmt.get_pattern ().get (), true);
  HIR::Type *type = stmt.has_type ()
		      ? ASTLoweringType::translate (stmt.get_type ().get ())
		      : nullptr;
  HIR::Expr *init_expression
    = stmt.has_init_expr ()
	? ASTLoweringExpr::translate (stmt.get_init_expr ().get ())
	: nullptr;

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, stmt.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  translated
    = new HIR::LetStmt (mapping, std::unique_ptr<HIR::Pattern> (variables),
			std::unique_ptr<HIR::Expr> (init_expression),
			std::unique_ptr<HIR::Type> (type),
			stmt.get_outer_attrs (), stmt.get_locus ());
}

void
ASTLoweringStmt::visit (AST::TupleStruct &struct_decl)
{
  translated = ASTLoweringItem::translate (&struct_decl);
}

void
ASTLoweringStmt::visit (AST::StructStruct &struct_decl)
{
  translated = ASTLoweringItem::translate (&struct_decl);
}

void
ASTLoweringStmt::visit (AST::Union &union_decl)
{
  translated = ASTLoweringItem::translate (&union_decl);
}

void
ASTLoweringStmt::visit (AST::Enum &enum_decl)
{
  translated = ASTLoweringItem::translate (&enum_decl);
}

void
ASTLoweringStmt::visit (AST::EmptyStmt &empty)
{
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, empty.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::EmptyStmt (mapping, empty.get_locus ());
}

void
ASTLoweringStmt::visit (AST::Function &function)
{
  translated = ASTLoweringItem::translate (&function);
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
  translated = ASTLoweringItem::translate (&trait);
}

void
ASTLoweringStmt::visit (AST::InherentImpl &impl_block)
{
  translated = ASTLoweringItem::translate (&impl_block);
}

void
ASTLoweringStmt::visit (AST::TraitImpl &impl_block)
{
  translated = ASTLoweringItem::translate (&impl_block);
}

} // namespace HIR
} // namespace Rust
