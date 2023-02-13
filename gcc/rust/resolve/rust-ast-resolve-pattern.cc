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
	    PatternDeclaration::go (inner_pattern.get (), type);
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
