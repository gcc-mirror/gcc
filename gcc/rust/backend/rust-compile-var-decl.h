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

#ifndef RUST_COMPILE_VAR_DECL
#define RUST_COMPILE_VAR_DECL

#include "rust-compile-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Compile {

class CompileVarDecl : public HIRCompileBase, public HIR::HIRPatternVisitor
{
  using HIR::HIRPatternVisitor::visit;

public:
  static std::vector<Bvariable *> compile (tree fndecl, tree translated_type,
					   HIR::Pattern *pattern, Context *ctx)
  {
    CompileVarDecl compiler (ctx, fndecl, translated_type);
    pattern->accept_vis (compiler);
    return compiler.vars;
  }

  void visit (HIR::IdentifierPattern &pattern) override
  {
    if (!pattern.is_mut ())
      translated_type = Backend::immutable_type (translated_type);

    tree bind_tree = ctx->peek_enclosing_scope ();
    std::string identifier = pattern.get_identifier ().as_string ();
    tree decl
      = build_decl (pattern.get_locus (), VAR_DECL,
		    Backend::get_identifier_node (identifier), translated_type);
    DECL_CONTEXT (decl) = fndecl;

    gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
    tree block_tree = BIND_EXPR_BLOCK (bind_tree);
    gcc_assert (TREE_CODE (block_tree) == BLOCK);
    DECL_CHAIN (decl) = BLOCK_VARS (block_tree);
    BLOCK_VARS (block_tree) = decl;
    BIND_EXPR_VARS (bind_tree) = BLOCK_VARS (block_tree);

    rust_preserve_from_gc (decl);
    Bvariable *var = new Bvariable (decl);

    HirId stmt_id = pattern.get_mappings ().get_hirid ();
    ctx->insert_var_decl (stmt_id, var);

    vars.push_back (var);
  }

  void visit (HIR::TuplePattern &pattern) override
  {
    switch (pattern.get_items ()->get_item_type ())
      {
	case HIR::TuplePatternItems::ItemType::MULTIPLE: {
	  rust_assert (TREE_CODE (translated_type) == RECORD_TYPE);
	  auto &items = static_cast<HIR::TuplePatternItemsMultiple &> (
	    *pattern.get_items ());

	  size_t offs = 0;
	  for (auto &sub : items.get_patterns ())
	    {
	      tree sub_ty = error_mark_node;
	      tree field = TYPE_FIELDS (translated_type);
	      for (size_t i = 0; i < offs; i++)
		{
		  field = DECL_CHAIN (field);
		  gcc_assert (field != NULL_TREE);
		}
	      sub_ty = TREE_TYPE (field);
	      CompileVarDecl::compile (fndecl, sub_ty, sub.get (), ctx);
	      offs++;
	    }
	}
	break;

      default:
	break;
      }
  }

  // Empty visit for unused Pattern HIR nodes.
  void visit (HIR::AltPattern &) override {}
  void visit (HIR::LiteralPattern &) override {}
  void visit (HIR::PathInExpression &) override {}
  void visit (HIR::QualifiedPathInExpression &) override {}
  void visit (HIR::RangePattern &) override {}
  void visit (HIR::ReferencePattern &) override {}
  void visit (HIR::SlicePattern &) override {}
  void visit (HIR::StructPattern &) override {}
  void visit (HIR::TupleStructPattern &) override {}
  void visit (HIR::WildcardPattern &) override {}

private:
  CompileVarDecl (Context *ctx, tree fndecl, tree translated_type)
    : HIRCompileBase (ctx), fndecl (fndecl), translated_type (translated_type)
  {}

  tree fndecl;
  tree translated_type;

  std::vector<Bvariable *> vars;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_VAR_DECL
