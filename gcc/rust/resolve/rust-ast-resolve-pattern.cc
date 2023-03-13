// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-ast-resolve-pattern.h"
#include "rust-ast-resolve-path.h"
#include "rust-ast-resolve-expr.h"

namespace Rust {
namespace Resolver {

void
PatternDeclaration::go (AST::Pattern *pattern, Rib::ItemType type)
{
  std::vector<PatternBinding> bindings
    = {PatternBinding (PatternBoundCtx::Product, std::set<Identifier> ())};
  PatternDeclaration resolver (bindings, type);
  pattern->accept_vis (resolver);
};

void
PatternDeclaration::go (AST::Pattern *pattern, Rib::ItemType type,
			std::vector<PatternBinding> &bindings)
{
  PatternDeclaration resolver (bindings, type);
  pattern->accept_vis (resolver);
}

void
PatternDeclaration::visit (AST::IdentifierPattern &pattern)
{
  bool has_binding_ctx = bindings.size () > 0;
  rust_assert (has_binding_ctx);

  auto &binding_idents = bindings.back ().idents;

  bool current_ctx_is_product
    = bindings.back ().ctx == PatternBoundCtx::Product;
  bool identifier_is_product_bound
    = current_ctx_is_product
      && binding_idents.find (pattern.get_ident ()) != binding_idents.end ();

  if (identifier_is_product_bound)
    {
      if (type == Rib::ItemType::Param)
	{
	  rust_error_at (pattern.get_locus (), ErrorCode ("E0415"),
			 "identifier '%s' is bound more than once in the "
			 "same parameter list",
			 pattern.get_ident ().c_str ());
	}
      else
	{
	  rust_error_at (
	    pattern.get_locus (), ErrorCode ("E0416"),
	    "identifier '%s' is bound more than once in the same pattern",
	    pattern.get_ident ().c_str ());
	}

      return;
    }

  // if we have a duplicate id this then allows for shadowing correctly
  // as new refs to this decl will match back here so it is ok to overwrite
  resolver->get_name_scope ().insert (
    CanonicalPath::new_seg (pattern.get_node_id (), pattern.get_ident ()),
    pattern.get_node_id (), pattern.get_locus (), type);

  binding_idents.insert (pattern.get_ident ());
}

void
PatternDeclaration::visit (AST::GroupedPattern &pattern)
{
  pattern.get_pattern_in_parens ()->accept_vis (*this);
}

void
PatternDeclaration::visit (AST::ReferencePattern &pattern)
{
  pattern.get_referenced_pattern ()->accept_vis (*this);
}

void
PatternDeclaration::visit (AST::PathInExpression &pattern)
{
  ResolvePath::go (&pattern);
}

void
PatternDeclaration::visit (AST::TupleStructPattern &pattern)
{
  ResolvePath::go (&pattern.get_path ());

  std::unique_ptr<AST::TupleStructItems> &items = pattern.get_items ();
  switch (items->get_item_type ())
    {
      case AST::TupleStructItems::RANGE: {
	// TODO
	gcc_unreachable ();
      }
      break;

      case AST::TupleStructItems::NO_RANGE: {
	AST::TupleStructItemsNoRange &items_no_range
	  = static_cast<AST::TupleStructItemsNoRange &> (*items.get ());

	for (auto &inner_pattern : items_no_range.get_patterns ())
	  {
	    inner_pattern.get ()->accept_vis (*this);
	  }
      }
      break;
    }
}

void
PatternDeclaration::visit (AST::StructPattern &pattern)
{
  ResolvePath::go (&pattern.get_path ());

  auto &struct_pattern_elems = pattern.get_struct_pattern_elems ();
  for (auto &field : struct_pattern_elems.get_struct_pattern_fields ())
    {
      switch (field->get_item_type ())
	{
	  case AST::StructPatternField::ItemType::TUPLE_PAT: {
	    // TODO
	    gcc_unreachable ();
	  }
	  break;

	  case AST::StructPatternField::ItemType::IDENT_PAT: {
	    // TODO
	    gcc_unreachable ();
	  }
	  break;

	  case AST::StructPatternField::ItemType::IDENT: {
	    AST::StructPatternFieldIdent &ident
	      = static_cast<AST::StructPatternFieldIdent &> (*field.get ());

	    resolver->get_name_scope ().insert (
	      CanonicalPath::new_seg (ident.get_node_id (),
				      ident.get_identifier ()),
	      ident.get_node_id (), ident.get_locus ());
	  }
	  break;
	}
    }

  // TODO
  rust_assert (!struct_pattern_elems.has_etc ());
}

void
PatternDeclaration::visit (AST::TuplePattern &pattern)
{
  std::unique_ptr<AST::TuplePatternItems> &items = pattern.get_items ();
  switch (items->get_pattern_type ())
    {
      case AST::TuplePatternItems::TuplePatternItemType::MULTIPLE: {
	AST::TuplePatternItemsMultiple &ref
	  = *static_cast<AST::TuplePatternItemsMultiple *> (
	    pattern.get_items ().get ());

	for (auto &p : ref.get_patterns ())
	  p->accept_vis (*this);
      }
      break;

      case AST::TuplePatternItems::TuplePatternItemType::RANGED: {
	AST::TuplePatternItemsRanged &ref
	  = *static_cast<AST::TuplePatternItemsRanged *> (
	    pattern.get_items ().get ());

	for (auto &p : ref.get_lower_patterns ())
	  p->accept_vis (*this);
	for (auto &p : ref.get_upper_patterns ())
	  p->accept_vis (*this);
      }
      break;
    }
}

static void
resolve_range_pattern_bound (AST::RangePatternBound *bound)
{
  switch (bound->get_bound_type ())
    {
    case AST::RangePatternBound::RangePatternBoundType::LITERAL:
      // Nothing to resolve for a literal.
      break;

      case AST::RangePatternBound::RangePatternBoundType::PATH: {
	AST::RangePatternBoundPath &ref
	  = *static_cast<AST::RangePatternBoundPath *> (bound);

	ResolvePath::go (&ref.get_path ());
      }
      break;

      case AST::RangePatternBound::RangePatternBoundType::QUALPATH: {
	AST::RangePatternBoundQualPath &ref
	  = *static_cast<AST::RangePatternBoundQualPath *> (bound);

	ResolvePath::go (&ref.get_qualified_path ());
      }
      break;
    }
}

void
PatternDeclaration::visit (AST::RangePattern &pattern)
{
  resolve_range_pattern_bound (pattern.get_upper_bound ().get ());
  resolve_range_pattern_bound (pattern.get_lower_bound ().get ());
}

} // namespace Resolver
} // namespace Rust
