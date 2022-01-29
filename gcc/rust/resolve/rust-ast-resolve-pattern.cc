// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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
#include "rust-ast-resolve-expr.h"

namespace Rust {
namespace Resolver {

void
PatternDeclaration::visit (AST::PathInExpression &pattern)
{
  ResolvePath::go (&pattern, parent);
}

void
PatternDeclaration::visit (AST::TupleStructPattern &pattern)
{
  ResolvePath::go (&pattern.get_path (), parent);

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
	    PatternDeclaration::go (inner_pattern.get (),
				    inner_pattern->get_pattern_node_id ());
	  }
      }
      break;
    }
}

void
PatternDeclaration::visit (AST::StructPattern &pattern)
{
  ResolvePath::go (&pattern.get_path (), parent);

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
	    resolver->insert_new_definition (ident.get_node_id (),
					     Definition{ident.get_node_id (),
							ident.get_node_id ()});
	    resolver->mark_decl_mutability (ident.get_node_id (),
					    ident.is_mut ());
	  }
	  break;
	}
    }

  // TODO
  rust_assert (!struct_pattern_elems.has_etc ());
}

} // namespace Resolver
} // namespace Rust
