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

#include "rust-ast-lower-pattern.h"
#include "rust-ast-lower-expr.h"
#include "rust-system.h"

namespace Rust {
namespace HIR {

ASTLoweringPattern::ASTLoweringPattern ()
  : translated (nullptr), is_let_top_level (false)
{}

HIR::Pattern *
ASTLoweringPattern::translate (AST::Pattern &pattern, bool is_let_top_level)
{
  ASTLoweringPattern resolver;
  resolver.is_let_top_level = is_let_top_level;
  pattern.accept_vis (resolver);

  rust_assert (resolver.translated != nullptr);

  resolver.mappings.insert_hir_pattern (resolver.translated);
  resolver.mappings.insert_location (
    resolver.translated->get_mappings ().get_hirid (), pattern.get_locus ());

  return resolver.translated;
}

void
ASTLoweringPattern::visit (AST::IdentifierPattern &pattern)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  std::unique_ptr<Pattern> subpattern;
  if (pattern.has_subpattern ())
    {
      subpattern = std::unique_ptr<Pattern> (
	ASTLoweringPattern::translate (pattern.get_subpattern ()));
    }
  translated
    = new HIR::IdentifierPattern (mapping, pattern.get_ident (),
				  pattern.get_locus (), pattern.get_is_ref (),
				  pattern.get_is_mut () ? Mutability::Mut
							: Mutability::Imm,
				  std::move (subpattern));
}

void
ASTLoweringPattern::visit (AST::PathInExpression &pattern)
{
  translated = ASTLowerPathInExpression::translate (pattern);
}

void
ASTLoweringPattern::visit (AST::TupleStructPattern &pattern)
{
  HIR::PathInExpression *path
    = ASTLowerPathInExpression::translate (pattern.get_path ());

  TupleStructItems *lowered = nullptr;
  auto &items = pattern.get_items ();
  switch (items.get_item_type ())
    {
    case AST::TupleStructItems::HAS_REST:
      {
	AST::TupleStructItemsHasRest &items_has_rest
	  = static_cast<AST::TupleStructItemsHasRest &> (items);

	std::vector<std::unique_ptr<HIR::Pattern>> lower_patterns;
	lower_patterns.reserve (items_has_rest.get_lower_patterns ().size ());
	for (auto &pattern_member : items_has_rest.get_lower_patterns ())
	  {
	    lower_patterns.emplace_back (
	      ASTLoweringPattern::translate (*pattern_member));
	  }

	std::vector<std::unique_ptr<HIR::Pattern>> upper_patterns;
	upper_patterns.reserve (items_has_rest.get_upper_patterns ().size ());
	for (auto &pattern_member : items_has_rest.get_upper_patterns ())
	  {
	    upper_patterns.emplace_back (
	      ASTLoweringPattern::translate (*pattern_member));
	  }

	lowered = new HIR::TupleStructItemsHasRest (std::move (lower_patterns),
						    std::move (upper_patterns));
      }
      break;

    case AST::TupleStructItems::NO_REST:
      {
	AST::TupleStructItemsNoRest &items_no_rest
	  = static_cast<AST::TupleStructItemsNoRest &> (items);

	std::vector<std::unique_ptr<HIR::Pattern>> patterns;
	patterns.reserve (items_no_rest.get_patterns ().size ());

	for (auto &inner_pattern : items_no_rest.get_patterns ())
	  patterns.emplace_back (
	    ASTLoweringPattern::translate (*inner_pattern));

	lowered = new HIR::TupleStructItemsNoRest (std::move (patterns));
      }
      break;
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::TupleStructPattern (
    mapping, *path, std::unique_ptr<HIR::TupleStructItems> (lowered));
}

void
ASTLoweringPattern::visit (AST::StructPattern &pattern)
{
  HIR::PathInExpression *path
    = ASTLowerPathInExpression::translate (pattern.get_path ());

  auto &raw_elems = pattern.get_struct_pattern_elems ();

  std::vector<std::unique_ptr<HIR::StructPatternField>> fields;
  for (auto &field : raw_elems.get_struct_pattern_fields ())
    {
      HIR::StructPatternField *f = nullptr;
      switch (field->get_item_type ())
	{
	case AST::StructPatternField::ItemType::TUPLE_PAT:
	  {
	    auto &tuple
	      = static_cast<AST::StructPatternFieldTuplePat &> (*field);

	    auto crate_num = mappings.get_current_crate ();
	    Analysis::NodeMapping mapping (crate_num, tuple.get_node_id (),
					   mappings.get_next_hir_id (crate_num),
					   UNKNOWN_LOCAL_DEFID);

	    std::unique_ptr<HIR::Pattern> pat (
	      ASTLoweringPattern::translate (tuple.get_index_pattern ()));

	    f = new HIR::StructPatternFieldTuplePat (mapping,
						     tuple.get_index (),
						     std::move (pat),
						     tuple.get_outer_attrs (),
						     tuple.get_locus ());
	  }
	  break;

	case AST::StructPatternField::ItemType::IDENT_PAT:
	  {
	    AST::StructPatternFieldIdentPat &ident
	      = static_cast<AST::StructPatternFieldIdentPat &> (*field);

	    auto crate_num = mappings.get_current_crate ();
	    Analysis::NodeMapping mapping (crate_num, ident.get_node_id (),
					   mappings.get_next_hir_id (crate_num),
					   UNKNOWN_LOCAL_DEFID);

	    std::unique_ptr<HIR::Pattern> pat (
	      ASTLoweringPattern::translate (ident.get_ident_pattern ()));

	    f = new HIR::StructPatternFieldIdentPat (mapping,
						     ident.get_identifier (),
						     std::move (pat),
						     ident.get_outer_attrs (),
						     ident.get_locus ());
	  }
	  break;

	case AST::StructPatternField::ItemType::IDENT:
	  {
	    AST::StructPatternFieldIdent &ident
	      = static_cast<AST::StructPatternFieldIdent &> (*field.get ());

	    auto crate_num = mappings.get_current_crate ();
	    Analysis::NodeMapping mapping (crate_num, ident.get_node_id (),
					   mappings.get_next_hir_id (crate_num),
					   UNKNOWN_LOCAL_DEFID);

	    f = new HIR::StructPatternFieldIdent (
	      mapping, ident.get_identifier (), ident.is_ref (),
	      ident.is_mut () ? Mutability::Mut : Mutability::Imm,
	      ident.get_outer_attrs (), ident.get_locus ());
	  }
	  break;
	}

      // insert the reverse mappings and locations
      auto field_id = f->get_mappings ().get_hirid ();
      auto field_node_id = f->get_mappings ().get_nodeid ();
      mappings.insert_location (field_id, f->get_locus ());
      mappings.insert_node_to_hir (field_node_id, field_id);

      // add it to the lowered fields list
      fields.emplace_back (f);
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  HIR::StructPatternElements elems (
    std::move (fields), pattern.get_struct_pattern_elems ().has_rest ());
  translated = new HIR::StructPattern (mapping, *path, std::move (elems));
}

void
ASTLoweringPattern::visit (AST::WildcardPattern &pattern)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::WildcardPattern (mapping, pattern.get_locus ());
}

void
ASTLoweringPattern::visit (AST::TuplePattern &pattern)
{
  std::unique_ptr<HIR::TuplePatternItems> items;
  switch (pattern.get_items ().get_item_type ())
    {
    case AST::TuplePatternItems::ItemType::NO_REST:
      {
	AST::TuplePatternItemsNoRest &ref
	  = static_cast<AST::TuplePatternItemsNoRest &> (pattern.get_items ());
	items = lower_tuple_pattern_multiple (ref);
      }
      break;

    case AST::TuplePatternItems::ItemType::HAS_REST:
      {
	AST::TuplePatternItemsHasRest &ref
	  = static_cast<AST::TuplePatternItemsHasRest &> (pattern.get_items ());
	items = lower_tuple_pattern_ranged (ref);
      }
      break;
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::TuplePattern (mapping, std::move (items), pattern.get_locus ());
}

void
ASTLoweringPattern::visit (AST::LiteralPattern &pattern)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  HIR::Literal l = lower_literal (pattern.get_literal ());
  translated
    = new HIR::LiteralPattern (mapping, std::move (l), pattern.get_locus (),
			       pattern.get_has_minus ());
}

void
ASTLoweringPattern::visit (AST::RangePattern &pattern)
{
  auto upper_bound = lower_range_pattern_bound (pattern.get_upper_bound ());
  auto lower_bound = lower_range_pattern_bound (pattern.get_lower_bound ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  bool is_inclusive = (pattern.get_range_kind () == AST::RangeKind::INCLUDED);

  translated = new HIR::RangePattern (mapping, std::move (lower_bound),
				      std::move (upper_bound),
				      pattern.get_locus (), is_inclusive);
}

void
ASTLoweringPattern::visit (AST::GroupedPattern &pattern)
{
  is_let_top_level = false;
  pattern.get_pattern_in_parens ().accept_vis (*this);
}

void
ASTLoweringPattern::visit (AST::ReferencePattern &pattern)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  HIR::Pattern *inner
    = ASTLoweringPattern::translate (pattern.get_referenced_pattern ());

  translated
    = new HIR::ReferencePattern (mapping, std::unique_ptr<HIR::Pattern> (inner),
				 pattern.get_is_mut () ? Mutability::Mut
						       : Mutability::Imm,
				 pattern.get_locus ());

  if (pattern.is_double_reference ())
    {
      Analysis::NodeMapping mapping2 (crate_num, pattern.get_node_id (),
				      mappings.get_next_hir_id (crate_num),
				      UNKNOWN_LOCAL_DEFID);
      translated
	= new HIR::ReferencePattern (mapping2,
				     std::unique_ptr<HIR::Pattern> (translated),
				     Mutability::Imm, pattern.get_locus ());
    }
}

void
ASTLoweringPattern::visit (AST::SlicePattern &pattern)
{
  std::unique_ptr<HIR::SlicePatternItems> items;

  switch (pattern.get_items ().get_item_type ())
    {
    case AST::SlicePatternItems::ItemType::NO_REST:
      {
	auto &ref
	  = static_cast<AST::SlicePatternItemsNoRest &> (pattern.get_items ());
	items = ASTLoweringBase::lower_slice_pattern_no_rest (ref);
      }
      break;
    case AST::SlicePatternItems::ItemType::HAS_REST:
      {
	auto &ref
	  = static_cast<AST::SlicePatternItemsHasRest &> (pattern.get_items ());
	items = ASTLoweringBase::lower_slice_pattern_has_rest (ref);
      }
      break;
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::SlicePattern (mapping, std::move (items), pattern.get_locus ());
}

void
ASTLoweringPattern::visit (AST::AltPattern &pattern)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  std::vector<std::unique_ptr<HIR::Pattern>> alts;
  alts.reserve (pattern.get_alts ().size ());

  for (auto &alt : pattern.get_alts ())
    alts.emplace_back (ASTLoweringPattern::translate (*alt));

  translated
    = new HIR::AltPattern (mapping, std::move (alts), pattern.get_locus ());

  if (is_let_top_level)
    {
      rich_location richloc (line_table, pattern.get_locus ());
      richloc.add_fixit_replace ("use an outer grouped pattern");
      rust_error_at (
	richloc, "top level or-patterns are not allowed for %<let%> bindings");
    }
}

} // namespace HIR
} // namespace Rust
