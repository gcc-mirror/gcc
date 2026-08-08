// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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
#include "rust-compile-type.h"
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

    if (pattern.has_subpattern ())
      {
	auto subpattern_vars
	  = CompileVarDecl::compile (fndecl, translated_type,
				     &pattern.get_subpattern (), ctx);
	vars.insert (vars.end (), subpattern_vars.begin (),
		     subpattern_vars.end ());
      }
  }

  void visit (HIR::TuplePattern &pattern) override
  {
    rust_assert (TREE_CODE (translated_type) == RECORD_TYPE);
    switch (pattern.get_items ().get_item_type ())
      {
      case HIR::TuplePatternItems::ItemType::NO_REST:
	{
	  auto &items_no_rest = static_cast<HIR::TuplePatternItemsNoRest &> (
	    pattern.get_items ());

	  tree field = TYPE_FIELDS (translated_type);
	  for (auto &sub : items_no_rest.get_patterns ())
	    {
	      gcc_assert (field != NULL_TREE);
	      tree sub_ty = TREE_TYPE (field);
	      CompileVarDecl::compile (fndecl, sub_ty, sub.get (), ctx);
	      field = DECL_CHAIN (field);
	    }
	}
	break;

      case HIR::TuplePatternItems::ItemType::HAS_REST:
	{
	  auto &items_has_rest = static_cast<HIR::TuplePatternItemsHasRest &> (
	    pattern.get_items ());

	  // count total fields in translated_type
	  size_t total_fields = 0;
	  for (tree t = TYPE_FIELDS (translated_type); t; t = DECL_CHAIN (t))
	    {
	      total_fields++;
	    }

	  // process lower patterns
	  tree field = TYPE_FIELDS (translated_type);
	  for (auto &sub : items_has_rest.get_lower_patterns ())
	    {
	      gcc_assert (field != NULL_TREE);
	      tree sub_ty = TREE_TYPE (field);
	      CompileVarDecl::compile (fndecl, sub_ty, sub.get (), ctx);
	      field = DECL_CHAIN (field);
	    }

	  // process upper patterns
	  if (!items_has_rest.get_upper_patterns ().empty ())
	    {
	      size_t upper_start
		= total_fields - items_has_rest.get_upper_patterns ().size ();
	      field = TYPE_FIELDS (translated_type);
	      for (size_t i = 0; i < upper_start; i++)
		{
		  field = DECL_CHAIN (field);
		  gcc_assert (field != NULL_TREE);
		}

	      for (auto &sub : items_has_rest.get_upper_patterns ())
		{
		  gcc_assert (field != NULL_TREE);
		  tree sub_ty = TREE_TYPE (field);
		  CompileVarDecl::compile (fndecl, sub_ty, sub.get (), ctx);
		  field = DECL_CHAIN (field);
		}
	    }
	}
	break;

      default:
	break;
      }
  }

  void visit (HIR::StructPattern &pattern) override
  {
    // lookup the type
    TyTy::BaseType *lookup = nullptr;
    bool ok = ctx->get_tyctx ()->lookup_type (
      pattern.get_path ().get_mappings ().get_hirid (), &lookup);
    rust_assert (ok);

    rust_assert (lookup->get_kind () == TyTy::TypeKind::ADT);
    TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (lookup);

    // only structs and single-variant enums are irrefutable, this check should
    // already be handled by type check
    rust_assert (adt->number_of_variants () == 1);

    int variant_index = 0;
    TyTy::VariantDef *variant = nullptr;
    if (adt->is_enum ())
      {
	// lookup the variant
	HirId variant_id = UNKNOWN_HIRID;
	bool ok = ctx->get_tyctx ()->lookup_variant_definition (
	  pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
	rust_assert (ok);

	ok = adt->lookup_variant_by_id (variant_id, &variant, &variant_index);
	rust_assert (ok);
      }
    else
      {
	variant = adt->get_variants ().at (0);
      }

    auto &struct_pattern_elems = pattern.get_struct_pattern_elems ();
    for (auto &field : struct_pattern_elems.get_struct_pattern_fields ())
      {
	switch (field->get_item_type ())
	  {
	  case HIR::StructPatternField::ItemType::TUPLE_PAT:
	    {
	      HIR::StructPatternFieldTuplePat &tuple_pat
		= static_cast<HIR::StructPatternFieldTuplePat &> (*field);
	      TyTy::StructFieldType *field_ty = nullptr;
	      ok = variant->lookup_field (std::to_string (
					    tuple_pat.get_index ()),
					  &field_ty, nullptr);
	      rust_assert (ok);
	      tree sub_ty
		= TyTyResolveCompile::compile (ctx,
					       field_ty->get_field_type ());
	      auto sub_vars
		= CompileVarDecl::compile (fndecl, sub_ty,
					   &tuple_pat.get_tuple_pattern (),
					   ctx);
	      vars.insert (vars.end (), sub_vars.begin (), sub_vars.end ());
	    }
	    break;
	  case HIR::StructPatternField::ItemType::IDENT_PAT:
	    {
	      HIR::StructPatternFieldIdentPat &ident_pat
		= static_cast<HIR::StructPatternFieldIdentPat &> (*field);
	      TyTy::StructFieldType *field_ty = nullptr;
	      ok = variant->lookup_field (
		ident_pat.get_identifier ().as_string (), &field_ty, nullptr);
	      rust_assert (ok);
	      tree sub_ty
		= TyTyResolveCompile::compile (ctx,
					       field_ty->get_field_type ());
	      auto sub_vars
		= CompileVarDecl::compile (fndecl, sub_ty,
					   &ident_pat.get_pattern (), ctx);
	      vars.insert (vars.end (), sub_vars.begin (), sub_vars.end ());
	    }
	    break;
	  case HIR::StructPatternField::ItemType::IDENT:
	    {
	      HIR::StructPatternFieldIdent &ident
		= static_cast<HIR::StructPatternFieldIdent &> (*field);
	      TyTy::StructFieldType *field_ty = nullptr;
	      ok = variant->lookup_field (ident.get_identifier ().as_string (),
					  &field_ty, nullptr);
	      rust_assert (ok);
	      tree sub_ty
		= TyTyResolveCompile::compile (ctx,
					       field_ty->get_field_type ());

	      // code below is pretty much copied from
	      // visit(IdentifierPattern) above
	      if (!ident.is_mut ())
		sub_ty = Backend::immutable_type (sub_ty);

	      tree bind_tree = ctx->peek_enclosing_scope ();
	      std::string identifier = ident.get_identifier ().as_string ();
	      tree decl = build_decl (ident.get_locus (), VAR_DECL,
				      Backend::get_identifier_node (identifier),
				      sub_ty);
	      DECL_CONTEXT (decl) = fndecl;
	      gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
	      tree block_tree = BIND_EXPR_BLOCK (bind_tree);
	      gcc_assert (TREE_CODE (block_tree) == BLOCK);
	      DECL_CHAIN (decl) = BLOCK_VARS (block_tree);
	      BLOCK_VARS (block_tree) = decl;
	      BIND_EXPR_VARS (bind_tree) = BLOCK_VARS (block_tree);
	      rust_preserve_from_gc (decl);
	      Bvariable *var = new Bvariable (decl);

	      HirId stmt_id = ident.get_mappings ().get_hirid ();
	      ctx->insert_var_decl (stmt_id, var);
	      vars.push_back (var);
	    }
	    break;
	  }
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
