// Copyright (C) 2026 Free Software Foundation, Inc.

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

#include "rust-hir-pattern-abstract.h"
#include "rust-hir-pattern.h"
#include "rust-hir-item.h"
#include "optional.h"
#include "rust-tyty.h"
#include "rust-hir-type-check.h"
namespace Rust {
namespace HIR {

static bool
is_refutable_with_lookup (Pattern &pattern)
{
  auto context = Resolver::TypeCheckContext::get ();
  HirId hir_id = pattern.get_mappings ().get_hirid ();
  if (hir_id)
    {
      TyTy::BaseType *ty = nullptr;
      if (context->lookup_type (hir_id, &ty))
	{
	  if (pattern.is_refutable (*ty))
	    return true;
	}
      else
	{
	  rust_internal_error_at (
	    pattern.get_locus (),
	    "failed to lookup hir item during refutability checks");
	  return true;
	}
    }
  return false;
}

bool
PathPattern::is_refutable (const TyTy::BaseType &scrutinee) const
{
  rust_assert (kind != Kind::LangItem);

  auto &mappings = Analysis::Mappings::get ();
  HirId hir_id = get_final_segment ().get_mappings ().get_hirid ();
  if (hir_id)
    {
      auto item = mappings.lookup_hir_item (hir_id);
      if (item && item.value ()->get_item_kind () == Item::ItemKind::Constant)
	{
	  return true;
	}
      else
	{
	  rust_internal_error_at (
	    get_locus (),
	    "failed to lookup hir item during refutability checks");
	  return true;
	}
    }

  // A path pattern is irrefutable if it corresponds to an enum with one variant
  if (scrutinee.get_kind () == TyTy::TypeKind::ADT)
    {
      const auto &adt = static_cast<const TyTy::ADTType &> (scrutinee);
      if (adt.is_enum ())
	{
	  return adt.number_of_variants () > 1;
	}
    }
  // cannot have a Path that is neither a constant or an enum-like ADT
  rust_unreachable ();
}

bool
SlicePattern::is_refutable (const TyTy::BaseType &scrutinee) const
{
  // A slice pattern is refutable if the scrutinee is a dynamic slice,
  // and the pattern contains anything other than just a rest pattern
  if (scrutinee.get_kind () == TyTy::TypeKind::SLICE)
    if (items->get_item_type () == SlicePatternItems::ItemType::HAS_REST)
      {
	const auto &items_has_rest
	  = static_cast<const SlicePatternItemsHasRest &> (*items);
	if (items_has_rest.get_lower_patterns ().empty ()
	    && items_has_rest.get_upper_patterns ().empty ())
	  return false;
      }

  rust_assert (scrutinee.get_kind () == TyTy::TypeKind::ARRAY);
  const auto &arr = static_cast<const TyTy::ArrayType &> (scrutinee);
  switch (items->get_item_type ())
    {
    case SlicePatternItems::ItemType::NO_REST:
      {
	const auto &items_no_rest
	  = static_cast<const SlicePatternItemsNoRest &> (*items);
	const auto *capacity_ty = arr.get_capacity ();
	rust_assert (capacity_ty->get_kind () == TyTy::TypeKind::CONST);
	auto *capacity_const = capacity_ty->as_const_type ();
	rust_assert (capacity_const->const_kind ()
		     == TyTy::BaseConstType::ConstKind::Value);
	auto &capacity_value
	  = *static_cast<const TyTy::ConstValueType *> (capacity_const);
	auto cap_tree = capacity_value.get_value ();
	rust_assert (!error_operand_p (cap_tree));
	size_t cap = (size_t) wi::to_wide (cap_tree).to_uhwi ();

	if (items_no_rest.get_patterns ().size () != cap)
	  return true;
	for (const auto &pattern : items_no_rest.get_patterns ())
	  if (is_refutable_with_lookup (*pattern))
	    {
	      return true;
	    }
	break;
      }
    case SlicePatternItems::ItemType::HAS_REST:
      {
	const auto &items_has_rest
	  = static_cast<const SlicePatternItemsHasRest &> (*items);
	size_t bound_patterns_count
	  = items_has_rest.get_lower_patterns ().size ()
	    + items_has_rest.get_upper_patterns ().size ();
	auto *capacity_ty = arr.get_capacity ();
	rust_assert (capacity_ty->get_kind () == TyTy::TypeKind::CONST);
	auto *capacity_const = capacity_ty->as_const_type ();
	rust_assert (capacity_const->const_kind ()
		     == TyTy::BaseConstType::ConstKind::Value);
	auto &capacity_value
	  = *static_cast<const TyTy::ConstValueType *> (capacity_const);
	auto cap_tree = capacity_value.get_value ();
	rust_assert (!error_operand_p (cap_tree));
	size_t cap = (size_t) wi::to_wide (cap_tree).to_uhwi ();

	if (bound_patterns_count > cap)
	  return true;
	for (const auto &pattern : items_has_rest.get_lower_patterns ())
	  if (is_refutable_with_lookup (*pattern))
	    {
	      return true;
	    }
	for (const auto &pattern : items_has_rest.get_upper_patterns ())
	  if (is_refutable_with_lookup (*pattern))
	    {
	      return true;
	    }
	break;
      }
    }
  return false;
}

bool
TupleStructPattern::is_refutable (const TyTy::BaseType &scrutinee) const
{
  // A tuple struct pattern corresponding to an enum with multiple variants is
  // always refutable
  if (scrutinee.get_kind () == TyTy::TypeKind::ADT)
    {
      const auto &adt = static_cast<const TyTy::ADTType &> (scrutinee);
      if (adt.is_enum () && adt.number_of_variants () > 1)
	{
	  return true;
	}
    }
  // We need to also check the refutability of each item's pattern in the tuple
  // struct
  switch (items->get_item_type ())
    {
    case TupleStructItems::ItemType::NO_REST:
      for (const auto &pattern :
	   static_cast<TupleStructItemsNoRest &> (*items).get_patterns ())
	if (is_refutable_with_lookup (*pattern))
	  {
	    return true;
	  }
      break;
    case TupleStructItems::ItemType::HAS_REST:
      auto &items_has_rest = static_cast<TupleStructItemsHasRest &> (*items);
      for (const auto &pattern : items_has_rest.get_lower_patterns ())
	if (is_refutable_with_lookup (*pattern))
	  {
	    return true;
	  }
      for (const auto &pattern : items_has_rest.get_upper_patterns ())
	if (is_refutable_with_lookup (*pattern))
	  {
	    return true;
	  }
      break;
    }
  return false;
}

bool
TuplePattern::is_refutable (const TyTy::BaseType &scrutinee) const
{
  const auto *destructured = scrutinee.destructure ();
  rust_assert (destructured->get_kind () == TyTy::TypeKind::TUPLE);
  const auto &tup = static_cast<const TyTy::TupleType &> (*destructured);

  switch (items->get_item_type ())
    {
    case TuplePatternItems::ItemType::NO_REST:
      {
	auto &no_rest = static_cast<TuplePatternItemsNoRest &> (*items);
	const auto &patterns = no_rest.get_patterns ();
	for (size_t i = 0; i < patterns.size (); i++)
	  if (patterns[i]->is_refutable (*tup.get_field (i)))
	    return true;
	break;
      }
    case TuplePatternItems::ItemType::HAS_REST:
      {
	auto &has_rest = static_cast<TuplePatternItemsHasRest &> (*items);
	const auto &lower = has_rest.get_lower_patterns ();
	const auto &upper = has_rest.get_upper_patterns ();
	for (size_t i = 0; i < lower.size (); i++)
	  if (lower[i]->is_refutable (*tup.get_field (i)))
	    return true;
	size_t base = tup.get_fields ().size () - upper.size ();
	for (size_t i = 0; i < upper.size (); i++)
	  if (upper[i]->is_refutable (*tup.get_field (base + i)))
	    return true;
	break;
      }
    }
  return false;
}
bool
IdentifierPattern::is_refutable (const TyTy::BaseType &scrutinee) const
{
  if (has_subpattern ())
    {
      auto context = Resolver::TypeCheckContext::get ();
      HirId hir_id = subpattern->get_mappings ().get_hirid ();
      if (hir_id)
	{
	  TyTy::BaseType *ty = nullptr;
	  if (context->lookup_type (hir_id, &ty))
	    {
	      return subpattern->is_refutable (*ty);
	    }
	  else
	    {
	      rust_internal_error_at (
		get_locus (),
		"failed to lookup hir item during refutability checks");
	      return true;
	    }
	}
    }

  return false;
}

bool
ReferencePattern::is_refutable (const TyTy::BaseType &scrutinee) const
{
  const auto *inner = scrutinee.destructure ();

  rust_assert (inner->get_kind () == TyTy::TypeKind::REF);
  const auto &ref = static_cast<const TyTy::ReferenceType &> (*inner);
  return pattern->is_refutable (*ref.get_base ());
}

bool
StructPattern::is_refutable (const TyTy::BaseType &scrutinee) const
{
  const auto *inner = scrutinee.destructure ();
  rust_assert (inner->get_kind () == TyTy::TypeKind::ADT);
  const auto &adt = static_cast<const TyTy::ADTType &> (*inner);
  rust_assert (!adt.is_enum ());
  TyTy::VariantDef *variant = adt.get_variants ().at (0);

  for (const auto &field : elems.get_struct_pattern_fields ())
    {
      switch (field->get_item_type ())
	{
	case StructPatternField::ItemType::TUPLE_PAT:
	  {
	    const auto &tuple_field
	      = static_cast<const StructPatternFieldTuplePat &> (*field);
	    TyTy::StructFieldType *field_ty
	      = variant->get_field_at_index (tuple_field.get_index ());
	    if (tuple_field.get_tuple_pattern ().is_refutable (
		  *field_ty->get_field_type ()))
	      return true;
	    break;
	  }
	case StructPatternField::ItemType::IDENT_PAT:
	  {
	    const auto &ident_field
	      = static_cast<const StructPatternFieldIdentPat &> (*field);
	    TyTy::StructFieldType *field_ty = nullptr;
	    bool found = variant->lookup_field (
	      ident_field.get_identifier ().as_string (), &field_ty, nullptr);
	    rust_assert (found);
	    if (ident_field.get_pattern ().is_refutable (
		  *field_ty->get_field_type ()))
	      return true;
	    break;
	  }
	case StructPatternField::ItemType::IDENT:
	  break;
	}
    }
  return false;
}
} // namespace HIR

} // namespace Rust
