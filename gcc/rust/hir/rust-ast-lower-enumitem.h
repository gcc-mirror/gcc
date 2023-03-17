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

#ifndef RUST_AST_LOWER_ENUMITEM
#define RUST_AST_LOWER_ENUMITEM

#include "rust-ast-lower.h"
#include "rust-diagnostics.h"

#include "rust-ast-lower-base.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower-expr.h"
#include "rust-hir-full-decls.h"

namespace Rust {
namespace HIR {

class ASTLoweringEnumItem : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::EnumItem *translate (AST::EnumItem *item)
  {
    ASTLoweringEnumItem resolver;
    item->accept_vis (resolver);

    rust_assert (resolver.translated != nullptr);

    auto hirid = resolver.translated->get_mappings ().get_hirid ();
    auto defid = resolver.translated->get_mappings ().get_defid ();

    resolver.mappings->insert_defid_mapping (defid, resolver.translated);
    resolver.mappings->insert_location (hirid,
					resolver.translated->get_locus ());

    return resolver.translated;
  }

  void visit (AST::EnumItem &item) override
  {
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, item.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    if (item.has_visibility ())
      rust_error_at (item.get_locus (),
		     "visibility qualifier %qs not allowed on enum item",
		     item.get_visibility ().as_string ().c_str ());
    translated = new HIR::EnumItem (mapping, item.get_identifier (),
				    item.get_outer_attrs (), item.get_locus ());
  }

  void visit (AST::EnumItemTuple &item) override
  {
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, item.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    if (item.has_visibility ())
      rust_error_at (item.get_locus (),
		     "visibility qualifier %qs not allowed on enum item",
		     item.get_visibility ().as_string ().c_str ());

    std::vector<HIR::TupleField> fields;
    for (auto &field : item.get_tuple_fields ())
      {
	HIR::Visibility vis = translate_visibility (field.get_visibility ());
	HIR::Type *type
	  = ASTLoweringType::translate (field.get_field_type ().get ());

	auto crate_num = mappings->get_current_crate ();
	Analysis::NodeMapping field_mapping (
	  crate_num, field.get_node_id (),
	  mappings->get_next_hir_id (crate_num),
	  mappings->get_next_localdef_id (crate_num));

	HIR::TupleField translated_field (field_mapping,
					  std::unique_ptr<HIR::Type> (type),
					  vis, field.get_locus (),
					  field.get_outer_attrs ());
	fields.push_back (std::move (translated_field));
      }

    translated
      = new HIR::EnumItemTuple (mapping, item.get_identifier (),
				std::move (fields), item.get_outer_attrs (),
				item.get_locus ());
  }

  void visit (AST::EnumItemStruct &item) override
  {
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, item.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    if (item.has_visibility ())
      rust_error_at (item.get_locus (),
		     "visibility qualifier %qs not allowed on enum item",
		     item.get_visibility ().as_string ().c_str ());

    std::vector<HIR::StructField> fields;
    for (auto &field : item.get_struct_fields ())
      {
	HIR::Visibility vis = translate_visibility (field.get_visibility ());
	HIR::Type *type
	  = ASTLoweringType::translate (field.get_field_type ().get ());

	auto crate_num = mappings->get_current_crate ();
	Analysis::NodeMapping field_mapping (
	  crate_num, field.get_node_id (),
	  mappings->get_next_hir_id (crate_num),
	  mappings->get_next_localdef_id (crate_num));

	HIR::StructField translated_field (field_mapping,
					   field.get_field_name (),
					   std::unique_ptr<HIR::Type> (type),
					   vis, field.get_locus (),
					   field.get_outer_attrs ());

	if (struct_field_name_exists (fields, translated_field))
	  break;

	fields.push_back (std::move (translated_field));
      }

    translated
      = new HIR::EnumItemStruct (mapping, item.get_identifier (),
				 std::move (fields), item.get_outer_attrs (),
				 item.get_locus ());
  }

  void visit (AST::EnumItemDiscriminant &item) override
  {
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, item.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    if (item.has_visibility ())
      rust_error_at (item.get_locus (),
		     "visibility qualifier %qs not allowed on enum item",
		     item.get_visibility ().as_string ().c_str ());

    HIR::Expr *expr = ASTLoweringExpr::translate (item.get_expr ().get ());
    translated
      = new HIR::EnumItemDiscriminant (mapping, item.get_identifier (),
				       std::unique_ptr<HIR::Expr> (expr),
				       item.get_outer_attrs (),
				       item.get_locus ());
  }

private:
  ASTLoweringEnumItem () : translated (nullptr) {}

  HIR::EnumItem *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_ENUMITEM
