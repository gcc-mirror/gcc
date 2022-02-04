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

#include "rust-hir-type-check-pattern.h"
#include "rust-hir-type-check-expr.h"

namespace Rust {
namespace Resolver {

void
TypeCheckPattern::visit (HIR::PathInExpression &pattern)
{
  infered = TypeCheckExpr::Resolve (&pattern, false);
}

void
TypeCheckPattern::visit (HIR::TupleStructPattern &pattern)
{
  infered = TypeCheckExpr::Resolve (&pattern.get_path (), false);
  if (infered->get_kind () == TyTy::TypeKind::ERROR)
    return;

  rust_assert (infered->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (infered);
  rust_assert (adt->is_enum ());

  // what variant is this?
  HirId variant_id;
  bool ok = context->lookup_variant_definition (
    pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
  rust_assert (ok);

  TyTy::VariantDef *variant = nullptr;
  ok = adt->lookup_variant_by_id (variant_id, &variant);
  rust_assert (ok);

  // error[E0532]: expected tuple struct or tuple variant, found struct variant
  // `Foo::D`
  if (variant->get_variant_type () != TyTy::VariantDef::VariantType::TUPLE)
    {
      std::string variant_type
	= TyTy::VariantDef::variant_type_string (variant->get_variant_type ());

      rust_error_at (
	pattern.get_locus (),
	"expected tuple struct or tuple variant, found %s variant %<%s::%s%>",
	variant_type.c_str (), adt->get_name ().c_str (),
	variant->get_identifier ().c_str ());
      return;
    }

  // check the elements
  // error[E0023]: this pattern has 2 fields, but the corresponding tuple
  // variant has 1 field
  // error[E0023]: this pattern has 0 fields, but the corresponding tuple
  // variant has 1 field

  std::unique_ptr<HIR::TupleStructItems> &items = pattern.get_items ();
  switch (items->get_item_type ())
    {
      case HIR::TupleStructItems::RANGE: {
	// TODO
	gcc_unreachable ();
      }
      break;

      case HIR::TupleStructItems::NO_RANGE: {
	HIR::TupleStructItemsNoRange &items_no_range
	  = static_cast<HIR::TupleStructItemsNoRange &> (*items.get ());

	if (items_no_range.get_patterns ().size () != variant->num_fields ())
	  {
	    rust_error_at (pattern.get_locus (),
			   "this pattern has %lu fields but the corresponding "
			   "tuple variant has %lu field",
			   items_no_range.get_patterns ().size (),
			   variant->num_fields ());
	    // we continue on to try and setup the types as best we can for
	    // type checking
	  }

	// iterate the fields and set them up, I wish we had ZIP
	size_t i = 0;
	for (auto &pattern : items_no_range.get_patterns ())
	  {
	    if (i >= variant->num_fields ())
	      break;

	    TyTy::StructFieldType *field = variant->get_field_at_index (i++);
	    TyTy::BaseType *fty = field->get_field_type ();

	    // setup the type on this pattern type
	    context->insert_type (pattern->get_pattern_mappings (), fty);
	  }
      }
      break;
    }
}

void
TypeCheckPattern::visit (HIR::StructPattern &pattern)
{
  infered = TypeCheckExpr::Resolve (&pattern.get_path (), false);
  if (infered->get_kind () == TyTy::TypeKind::ERROR)
    return;

  rust_assert (infered->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (infered);
  rust_assert (adt->is_enum ());

  // what variant is this?
  HirId variant_id;
  bool ok = context->lookup_variant_definition (
    pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
  rust_assert (ok);

  TyTy::VariantDef *variant = nullptr;
  ok = adt->lookup_variant_by_id (variant_id, &variant);
  rust_assert (ok);

  // error[E0532]: expected tuple struct or tuple variant, found struct variant
  // `Foo::D`
  if (variant->get_variant_type () != TyTy::VariantDef::VariantType::STRUCT)
    {
      std::string variant_type
	= TyTy::VariantDef::variant_type_string (variant->get_variant_type ());
      rust_error_at (pattern.get_locus (),
		     "expected struct variant, found %s variant %s",
		     variant_type.c_str (),
		     variant->get_identifier ().c_str ());
      return;
    }

  // check the elements
  // error[E0027]: pattern does not mention fields `x`, `y`
  // error[E0026]: variant `Foo::D` does not have a field named `b`

  std::vector<std::string> named_fields;
  auto &struct_pattern_elems = pattern.get_struct_pattern_elems ();
  for (auto &field : struct_pattern_elems.get_struct_pattern_fields ())
    {
      switch (field->get_item_type ())
	{
	  case HIR::StructPatternField::ItemType::TUPLE_PAT: {
	    // TODO
	    gcc_unreachable ();
	  }
	  break;

	  case HIR::StructPatternField::ItemType::IDENT_PAT: {
	    // TODO
	    gcc_unreachable ();
	  }
	  break;

	  case HIR::StructPatternField::ItemType::IDENT: {
	    HIR::StructPatternFieldIdent &ident
	      = static_cast<HIR::StructPatternFieldIdent &> (*field.get ());

	    TyTy::StructFieldType *field = nullptr;
	    if (!variant->lookup_field (ident.get_identifier (), &field,
					nullptr))
	      {
		rust_error_at (ident.get_locus (),
			       "variant %s does not have a field named %s",
			       variant->get_identifier ().c_str (),
			       ident.get_identifier ().c_str ());
		break;
	      }
	    named_fields.push_back (ident.get_identifier ());

	    // setup the type on this pattern
	    TyTy::BaseType *fty = field->get_field_type ();
	    context->insert_type (ident.get_mappings (), fty);
	  }
	  break;
	}
    }

  if (named_fields.size () != variant->num_fields ())
    {
      std::map<std::string, bool> missing_names;

      // populate with all fields
      for (auto &field : variant->get_fields ())
	missing_names[field->get_name ()] = true;

      // then eliminate with named_fields
      for (auto &named : named_fields)
	missing_names.erase (named);

      // then get the list of missing names
      size_t i = 0;
      std::string missing_fields_str;
      for (auto it = missing_names.begin (); it != missing_names.end (); it++)
	{
	  bool has_next = (i + 1) < missing_names.size ();
	  missing_fields_str += it->first + (has_next ? ", " : "");
	  i++;
	}

      rust_error_at (pattern.get_locus (), "pattern does not mention fields %s",
		     missing_fields_str.c_str ());
    }
}

void
TypeCheckPattern::visit (HIR::WildcardPattern &pattern)
{
  // wildcard patterns within the MatchArm's are simply just the same type as
  // the parent
  infered = parent->clone ();
  infered->set_ref (pattern.get_pattern_mappings ().get_hirid ());
}

} // namespace Resolver
} // namespace Rust
