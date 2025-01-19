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

#include "rust-ast-resolve-pattern.h"
#include "rust-ast-resolve-path.h"

namespace Rust {
namespace Resolver {

void
PatternDeclaration::go (AST::Pattern &pattern, Rib::ItemType type)
{
  std::vector<PatternBinding> bindings
    = {PatternBinding (PatternBoundCtx::Product, std::set<Identifier> ())};
  PatternDeclaration::go (pattern, type, bindings);
}

void
PatternDeclaration::go (AST::Pattern &pattern, Rib::ItemType type,
			std::vector<PatternBinding> &bindings)
{
  PatternDeclaration resolver (bindings, type);
  pattern.accept_vis (resolver);

  for (auto &map_entry : resolver.missing_bindings)
    {
      auto ident = map_entry.first; // key
      auto info = map_entry.second; // value

      rust_error_at (info.get_locus (), ErrorCode::E0408,
		     "variable '%s' is not bound in all patterns",
		     ident.as_string ().c_str ());
    }

  for (auto &map_entry : resolver.inconsistent_bindings)
    {
      auto ident = map_entry.first; // key
      auto info = map_entry.second; // value

      rust_error_at (
	info.get_locus (), ErrorCode::E0409,
	"variable '%s' is bound inconsistently across pattern alternatives",
	ident.as_string ().c_str ());
    }
}

void
PatternDeclaration::visit (AST::IdentifierPattern &pattern)
{
  Mutability mut = pattern.get_is_mut () ? Mutability::Mut : Mutability::Imm;
  add_new_binding (pattern.get_ident (), pattern.get_node_id (),
		   BindingTypeInfo (mut, pattern.get_is_ref (),
				    pattern.get_locus ()));
}

void
PatternDeclaration::visit (AST::GroupedPattern &pattern)
{
  pattern.get_pattern_in_parens ().accept_vis (*this);
}

void
PatternDeclaration::visit (AST::ReferencePattern &pattern)
{
  pattern.get_referenced_pattern ().accept_vis (*this);
}

void
PatternDeclaration::visit (AST::PathInExpression &pattern)
{
  ResolvePath::go (pattern);
}

void
PatternDeclaration::visit (AST::TupleStructPattern &pattern)
{
  ResolvePath::go (pattern.get_path ());

  AST::TupleStructItems &items = pattern.get_items ();
  switch (items.get_item_type ())
    {
      case AST::TupleStructItems::RANGE: {
	// TODO
	rust_unreachable ();
      }
      break;

      case AST::TupleStructItems::NO_RANGE: {
	auto &items_no_range
	  = static_cast<AST::TupleStructItemsNoRange &> (items);

	for (auto &inner_pattern : items_no_range.get_patterns ())
	  {
	    inner_pattern->accept_vis (*this);
	  }
      }
      break;
    }
}

void
PatternDeclaration::visit (AST::StructPattern &pattern)
{
  ResolvePath::go (pattern.get_path ());

  auto &struct_pattern_elems = pattern.get_struct_pattern_elems ();
  for (auto &field : struct_pattern_elems.get_struct_pattern_fields ())
    {
      switch (field->get_item_type ())
	{
	  case AST::StructPatternField::ItemType::TUPLE_PAT: {
	    AST::StructPatternFieldTuplePat &tuple
	      = static_cast<AST::StructPatternFieldTuplePat &> (*field);

	    tuple.get_index_pattern ().accept_vis (*this);
	  }
	  break;

	  case AST::StructPatternField::ItemType::IDENT_PAT: {
	    AST::StructPatternFieldIdentPat &ident
	      = static_cast<AST::StructPatternFieldIdentPat &> (*field);

	    ident.get_ident_pattern ().accept_vis (*this);
	  }
	  break;

	  case AST::StructPatternField::ItemType::IDENT: {
	    auto &ident = static_cast<AST::StructPatternFieldIdent &> (*field);

	    Mutability mut
	      = ident.is_mut () ? Mutability::Mut : Mutability::Imm;

	    add_new_binding (ident.get_identifier (), ident.get_node_id (),
			     BindingTypeInfo (mut, ident.is_ref (),
					      ident.get_locus ()));
	  }
	  break;
	}
    }
}

void
PatternDeclaration::visit (AST::TuplePattern &pattern)
{
  auto &items = pattern.get_items ();
  switch (items.get_pattern_type ())
    {
      case AST::TuplePatternItems::TuplePatternItemType::MULTIPLE: {
	auto &ref = static_cast<AST::TuplePatternItemsMultiple &> (
	  pattern.get_items ());

	for (auto &p : ref.get_patterns ())
	  p->accept_vis (*this);
      }
      break;

      case AST::TuplePatternItems::TuplePatternItemType::RANGED: {
	auto &ref
	  = static_cast<AST::TuplePatternItemsRanged &> (pattern.get_items ());

	for (auto &p : ref.get_lower_patterns ())
	  p->accept_vis (*this);
	for (auto &p : ref.get_upper_patterns ())
	  p->accept_vis (*this);
      }
      break;
    }
}

void
PatternDeclaration::visit (AST::AltPattern &pattern)
{
  // push a new set of 'Or' bindings to the stack. Accounts for the
  // alternatives. e.g. in `p_0 | p_1`, bindings to the same identifier between
  // p_0 and p_1 shouldn't cause an error.
  bindings_with_ctx.push_back (
    PatternBinding (PatternBoundCtx::Or, std::set<Identifier> ()));

  // This is a hack to avoid creating a separate visitor class for the
  // consistency checks. We empty out the binding_info_map before each iteration
  // to separate between the alts' binding_maps. And right after the alt
  // visit...
  auto tmp_binding_map = binding_info_map;
  binding_info_map.clear ();

  std::vector<BindingMap> alts_binding_maps;

  for (auto &alt : pattern.get_alts ())
    {
      // before this visit, the binding_info_map is guaranteed to be empty
      rust_assert (binding_info_map.empty ());

      // push a new `Product` context to correctly reject multiple bindings
      // within this single alt.
      bindings_with_ctx.push_back (
	PatternBinding (PatternBoundCtx::Product, std::set<Identifier> ()));

      alt->accept_vis (*this);

      // ...the binding_info_map is (potentially) populated. We copy it to the
      // vector, and empty it out to be ready for the next iteration. And after
      // all the iterations are finished...
      alts_binding_maps.push_back (binding_info_map);
      binding_info_map.clear ();

      // Remove the last (i.e. `Product`) context and add the bindings from the
      // visited alt to the one before last (i.e. `Or`). Now (after checking
      // with the alt internally), the bindings from this alt will reside in the
      // `Or` context.
      auto last_bound_idents = bindings_with_ctx.back ().idents;
      bindings_with_ctx.pop_back ();

      for (auto &ident : last_bound_idents)
	{
	  bindings_with_ctx.back ().idents.insert (ident);
	}
    }

  // Now we can finally check for consistency.
  check_bindings_consistency (alts_binding_maps);

  // Now we remove the `Or` context we pushed earlier.
  // e.g. in `(a, (p_0 | p_1), c)`: after finishing up inside the alt pattern,
  // we return to the tuple (`Product`) context and push the new bindings.
  auto idents = bindings_with_ctx.back ().idents;
  bindings_with_ctx.pop_back ();
  for (auto &ident : idents)
    bindings_with_ctx.back ().idents.insert (ident.as_string ());

  // ...we repopulate the binding_info_map correctly (the initial bindings
  // stored in the tmp_binding_map + all the bindings from all the alts)
  binding_info_map = tmp_binding_map;
  for (auto &alt_map : alts_binding_maps)
    for (auto &map_entry : alt_map)
      binding_info_map.insert (map_entry);
}

void
PatternDeclaration::add_new_binding (Identifier ident, NodeId node_id,
				     BindingTypeInfo info)
{
  bool has_binding_ctx = bindings_with_ctx.size () > 0;
  rust_assert (has_binding_ctx);

  bool identifier_or_bound = false, identifier_product_bound = false;

  for (auto binding : bindings_with_ctx)
    {
      bool identifier_bound_here
	= (binding.idents.find (ident) != binding.idents.end ());
      if (identifier_bound_here)
	{
	  identifier_product_bound |= binding.ctx == PatternBoundCtx::Product;
	  identifier_or_bound |= binding.ctx == PatternBoundCtx::Or;
	}
    }

  if (identifier_product_bound)
    {
      if (type == Rib::ItemType::Param)
	{
	  rust_error_at (info.get_locus (), ErrorCode::E0415,
			 "identifier '%s' is bound more than once in the "
			 "same parameter list",
			 ident.as_string ().c_str ());
	}
      else
	{
	  rust_error_at (
	    info.get_locus (), ErrorCode::E0416,
	    "identifier '%s' is bound more than once in the same pattern",
	    ident.as_string ().c_str ());
	}

      return;
    }

  if (!identifier_or_bound)
    {
      bindings_with_ctx.back ().idents.insert (ident);
      resolver->get_name_scope ().insert (
	CanonicalPath::new_seg (node_id, ident.as_string ()), node_id,
	info.get_locus (), type);
    }

  binding_info_map.insert ({ident, info});
}

// Verifies that all the alts in an AltPattern have the same set of bindings
// with the same mutability and reference states.
void
PatternDeclaration::check_bindings_consistency (
  std::vector<BindingMap> &binding_maps)
{
  for (size_t i = 0; i < binding_maps.size (); i++)
    {
      auto &outer_bindings_map = binding_maps[i];

      for (size_t j = 0; j < binding_maps.size (); j++)
	{
	  // skip comparing the current outer map with itself.
	  if (j == i)
	    continue;

	  auto &inner_bindings_map = binding_maps[j];

	  // iterate over the inner map entries and check if they exist in outer
	  // map
	  for (auto map_entry : inner_bindings_map)
	    {
	      auto ident = map_entry.first;	  // key
	      auto inner_info = map_entry.second; // value
	      bool ident_is_outer_bound = outer_bindings_map.count (ident);

	      if (!ident_is_outer_bound && !missing_bindings.count (ident))
		missing_bindings.insert ({ident, inner_info});

	      else if (outer_bindings_map[ident] != inner_info
		       && !inconsistent_bindings.count (ident))
		inconsistent_bindings.insert ({ident, inner_info});
	    }
	}
    }
}

static void
resolve_range_pattern_bound (AST::RangePatternBound &bound)
{
  switch (bound.get_bound_type ())
    {
    case AST::RangePatternBound::RangePatternBoundType::LITERAL:
      // Nothing to resolve for a literal.
      break;

      case AST::RangePatternBound::RangePatternBoundType::PATH: {
	auto &ref = static_cast<AST::RangePatternBoundPath &> (bound);

	ResolvePath::go (ref.get_path ());
      }
      break;

      case AST::RangePatternBound::RangePatternBoundType::QUALPATH: {
	auto &ref = static_cast<AST::RangePatternBoundQualPath &> (bound);

	ResolvePath::go (ref.get_qualified_path ());
      }
      break;
    }
}

void
PatternDeclaration::visit (AST::RangePattern &pattern)
{
  resolve_range_pattern_bound (pattern.get_upper_bound ());
  resolve_range_pattern_bound (pattern.get_lower_bound ());
}

void
PatternDeclaration::visit (AST::SlicePattern &pattern)
{
  for (auto &p : pattern.get_items ())
    {
      p->accept_vis (*this);
    }
}

} // namespace Resolver
} // namespace Rust
