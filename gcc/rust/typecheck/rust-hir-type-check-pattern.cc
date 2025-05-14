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

#include "rust-hir-type-check-pattern.h"
#include "rust-hir-pattern.h"
#include "rust-hir-type-check-expr.h"
#include "rust-type-util.h"
#include "rust-immutable-name-resolution-context.h"

// for flag_name_resolution_2_0
#include "options.h"

namespace Rust {
namespace Resolver {

TypeCheckPattern::TypeCheckPattern (TyTy::BaseType *parent)
  : TypeCheckBase (), parent (parent), infered (new TyTy::ErrorType (0))
{}

TyTy::BaseType *
TypeCheckPattern::Resolve (HIR::Pattern &pattern, TyTy::BaseType *parent)
{
  TypeCheckPattern resolver (parent);
  pattern.accept_vis (resolver);

  if (resolver.infered == nullptr)
    return new TyTy::ErrorType (pattern.get_mappings ().get_hirid ());

  resolver.context->insert_type (pattern.get_mappings (), resolver.infered);
  return resolver.infered;
}

void
TypeCheckPattern::visit (HIR::PathInExpression &pattern)
{
  // Pattern must be enum variants, sturcts, constants, or associated constansts
  TyTy::BaseType *pattern_ty = TypeCheckExpr::Resolve (pattern);

  NodeId ref_node_id = UNKNOWN_NODEID;
  bool maybe_item = false;

  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      if (auto id = nr_ctx.lookup (pattern.get_mappings ().get_nodeid ()))
	{
	  ref_node_id = *id;
	  maybe_item = true;
	}
    }
  else
    {
      maybe_item |= resolver->lookup_resolved_name (
	pattern.get_mappings ().get_nodeid (), &ref_node_id);
      maybe_item |= resolver->lookup_resolved_type (
	pattern.get_mappings ().get_nodeid (), &ref_node_id);
    }

  bool path_is_const_item = false;

  if (maybe_item)
    {
      tl::optional<HirId> definition_id
	= mappings.lookup_node_to_hir (ref_node_id);
      rust_assert (definition_id.has_value ());
      HirId def_id = definition_id.value ();

      tl::optional<HIR::Item *> hir_item = mappings.lookup_hir_item (def_id);
      // If the path refrerences an item, it must be constants or structs.
      if (hir_item.has_value ())
	{
	  HIR::Item *item = hir_item.value ();
	  if (item->get_item_kind () == HIR::Item::ItemKind::Constant)
	    {
	      path_is_const_item = true;
	    }
	  else if (item->get_item_kind () != HIR::Item::ItemKind::Struct)
	    {
	      HIR::Item *item = hir_item.value ();
	      std::string item_kind
		= HIR::Item::item_kind_string (item->get_item_kind ());

	      std::string path_buf;
	      for (size_t i = 0; i < pattern.get_segments ().size (); i++)
		{
		  HIR::PathExprSegment &seg = pattern.get_segments ().at (i);
		  path_buf += seg.as_string ();
		  if (i != pattern.get_segments ().size () - 1)
		    path_buf += "::";
		}

	      rich_location rich_locus (
		line_table, pattern.get_final_segment ().get_locus ());
	      rich_locus.add_fixit_replace (
		"not a unit struct, unit variant or constant");
	      rust_error_at (rich_locus, ErrorCode::E0532,
			     "expected unit struct, unit variant or constant, "
			     "found %s %<%s%>",
			     item_kind.c_str (), path_buf.c_str ());
	      return;
	    }
	}
    }

  // If the path is a constructor, it must be a unit struct or unit variants.
  if (!path_is_const_item && pattern_ty->get_kind () == TyTy::TypeKind::ADT)
    {
      TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (pattern_ty);
      rust_assert (adt->get_variants ().size () > 0);

      TyTy::VariantDef *variant = adt->get_variants ().at (0);
      if (adt->is_enum ())
	{
	  HirId variant_id = UNKNOWN_HIRID;
	  bool ok = context->lookup_variant_definition (
	    pattern.get_mappings ().get_hirid (), &variant_id);
	  rust_assert (ok);

	  ok = adt->lookup_variant_by_id (variant_id, &variant);
	  rust_assert (ok);
	}

      if (variant->get_variant_type () != TyTy::VariantDef::VariantType::NUM)
	{
	  std::string variant_type = TyTy::VariantDef::variant_type_string (
	    variant->get_variant_type ());

	  rich_location rich_locus (line_table,
				    pattern.get_final_segment ().get_locus ());
	  rich_locus.add_fixit_replace (
	    "not a unit struct, unit variant or constatnt");
	  rust_error_at (rich_locus, ErrorCode::E0532,
			 "expected unit struct, unit variant or constant, "
			 "found %s variant %<%s::%s%>",
			 variant_type.c_str (), adt->get_name ().c_str (),
			 variant->get_identifier ().c_str ());
	  return;
	}

      infered = pattern_ty;
    }
}

void
TypeCheckPattern::visit (HIR::TupleStructPattern &pattern)
{
  TyTy::BaseType *pattern_ty = TypeCheckExpr::Resolve (pattern.get_path ());
  if (pattern_ty->get_kind () != TyTy::TypeKind::ADT)
    {
      rust_error_at (
	pattern.get_locus (), ErrorCode::E0532,
	"expected tuple struct or tuple variant, found function %qs",
	pattern_ty->get_name ().c_str ());
      return;
    }

  infered = pattern_ty;
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (infered);
  rust_assert (adt->number_of_variants () > 0);

  TyTy::VariantDef *variant = adt->get_variants ().at (0);
  if (adt->is_enum ())
    {
      HirId variant_id = UNKNOWN_HIRID;
      bool ok = context->lookup_variant_definition (
	pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
      rust_assert (ok);

      ok = adt->lookup_variant_by_id (variant_id, &variant);
      rust_assert (ok);
    }

  // error[E0532]: expected tuple struct or tuple variant, found struct
  // variant `Foo::D`, E0532 by rustc 1.49.0 , E0164 by rustc 1.71.0
  if (variant->get_variant_type () != TyTy::VariantDef::VariantType::TUPLE)
    {
      std::string variant_type
	= TyTy::VariantDef::variant_type_string (variant->get_variant_type ());

      rich_location rich_locus (line_table, pattern.get_locus ());
      rich_locus.add_fixit_replace ("not a tuple struct or tuple variant");
      rust_error_at (
	rich_locus, ErrorCode::E0164,
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

  auto &items = pattern.get_items ();
  switch (items.get_item_type ())
    {
      case HIR::TupleStructItems::RANGED: {
	// TODO
	rust_unreachable ();
      }
      break;

      case HIR::TupleStructItems::MULTIPLE: {
	HIR::TupleStructItemsNoRange &items_no_range
	  = static_cast<HIR::TupleStructItemsNoRange &> (items);

	if (items_no_range.get_patterns ().size () != variant->num_fields ())
	  {
	    rust_error_at (
	      pattern.get_locus (), ErrorCode::E0023,
	      "this pattern has %lu fields but the corresponding "
	      "tuple variant has %lu field",
	      (unsigned long) items_no_range.get_patterns ().size (),
	      (unsigned long) variant->num_fields ());
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
	    context->insert_type (pattern->get_mappings (), fty);
	    TypeCheckPattern::Resolve (*pattern, fty);
	  }
      }
      break;
    }
}

void
emit_invalid_field_error (location_t loc, Rust::TyTy::VariantDef *variant,
			  const std::string &name)
{
  rust_error_at (loc, ErrorCode::E0026,
		 "variant %s does not have a field named %s",
		 variant->get_identifier ().c_str (), name.c_str ());
}

void
TypeCheckPattern::visit (HIR::StructPattern &pattern)
{
  TyTy::BaseType *pattern_ty = TypeCheckExpr::Resolve (pattern.get_path ());
  if (pattern_ty->get_kind () != TyTy::TypeKind::ADT)
    {
      rust_error_at (pattern.get_locus (),
		     "expected tuple struct/variant, found: %s",
		     pattern_ty->get_name ().c_str ());
      return;
    }

  infered = pattern_ty;
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (infered);
  if (adt->number_of_variants () == 0)
    {
      HIR::PathInExpression &path = pattern.get_path ();
      const AST::SimplePath &sp = path.as_simple_path ();
      rust_error_at (pattern.get_locus (), ErrorCode::E0574,
		     "expected struct, variant or union type, found enum %qs",
		     sp.as_string ().c_str ());
      return;
    }

  TyTy::VariantDef *variant = adt->get_variants ().at (0);
  if (adt->is_enum ())
    {
      HirId variant_id = UNKNOWN_HIRID;
      bool ok = context->lookup_variant_definition (
	pattern.get_path ().get_mappings ().get_hirid (), &variant_id);
      if (!ok)
	{
	  HIR::PathInExpression &path = pattern.get_path ();
	  const AST::SimplePath &sp = path.as_simple_path ();
	  rust_error_at (
	    pattern.get_locus (), ErrorCode::E0574,
	    "expected struct, variant or union type, found enum %qs",
	    sp.as_string ().c_str ());
	  return;
	}

      ok = adt->lookup_variant_by_id (variant_id, &variant);
      rust_assert (ok);
    }

  // error[E0532]: expected tuple struct or tuple variant, found struct
  // variant `Foo::D`
  if (variant->get_variant_type () != TyTy::VariantDef::VariantType::STRUCT)
    {
      std::string variant_type
	= TyTy::VariantDef::variant_type_string (variant->get_variant_type ());

      rich_location rich_locus (line_table, pattern.get_locus ());
      std::string rich_msg = "use the tuple variant pattern syntax instead "
			     + variant->get_identifier () + "(_)";
      rich_locus.add_fixit_replace (rich_msg.c_str ());

      rust_error_at (rich_locus, ErrorCode::E0769,
		     "%s variant %qs written as struct variant",
		     variant_type.c_str (),
		     variant->get_identifier ().c_str ());
      return;
    }

  std::vector<std::string> named_fields;
  auto &struct_pattern_elems = pattern.get_struct_pattern_elems ();
  for (auto &field : struct_pattern_elems.get_struct_pattern_fields ())
    {
      switch (field->get_item_type ())
	{
	  case HIR::StructPatternField::ItemType::TUPLE_PAT: {
	    // TODO
	    rust_unreachable ();
	  }
	  break;

	  case HIR::StructPatternField::ItemType::IDENT_PAT: {
	    HIR::StructPatternFieldIdentPat &ident
	      = static_cast<HIR::StructPatternFieldIdentPat &> (*field);

	    TyTy::StructFieldType *field = nullptr;
	    if (!variant->lookup_field (ident.get_identifier ().as_string (),
					&field, nullptr))
	      {
		emit_invalid_field_error (ident.get_locus (), variant,
					  ident.get_identifier ().as_string ());
		break;
	      }
	    named_fields.push_back (ident.get_identifier ().as_string ());

	    TyTy::BaseType *fty = field->get_field_type ();
	    TypeCheckPattern::Resolve (ident.get_pattern (), fty);
	  }
	  break;

	  case HIR::StructPatternField::ItemType::IDENT: {
	    HIR::StructPatternFieldIdent &ident
	      = static_cast<HIR::StructPatternFieldIdent &> (*field);

	    TyTy::StructFieldType *field = nullptr;
	    if (!variant->lookup_field (ident.get_identifier ().as_string (),
					&field, nullptr))
	      {
		emit_invalid_field_error (ident.get_locus (), variant,
					  ident.get_identifier ().as_string ());
		break;
	      }
	    named_fields.push_back (ident.get_identifier ().as_string ());

	    // setup the type on this pattern
	    TyTy::BaseType *fty = field->get_field_type ();
	    context->insert_type (ident.get_mappings (), fty);
	  }
	  break;
	}
    }

  // check the elements
  if (adt->is_union ())
    {
      auto &struct_pattern_elems = pattern.get_struct_pattern_elems ();
      if (struct_pattern_elems.get_struct_pattern_fields ().size () != 1)
	rust_error_at (pattern.get_locus (),
		       "union patterns should have exactly one field");

      else
	{
	  switch (struct_pattern_elems.get_struct_pattern_fields ()
		    .at (0)
		    ->get_item_type ())
	    {
	    case HIR::StructPatternField::ItemType::IDENT:
	    case HIR::StructPatternField::ItemType::IDENT_PAT:
	      break;
	      default: {
		auto first_elem
		  = struct_pattern_elems.get_struct_pattern_fields ()
		      .at (0)
		      ->as_string ();
		rust_error_at (pattern.get_locus (),
			       "%qs cannot be used in union patterns",
			       first_elem.c_str ());
	      }
	    }
	}
    }
  else
    {
      // Expects enum struct or struct struct.
      // error[E0027]: pattern does not mention fields `x`, `y`
      // error[E0026]: variant `Foo::D` does not have a field named `b`
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
	  for (auto it = missing_names.begin (); it != missing_names.end ();
	       it++)
	    {
	      bool has_next = (i + 1) < missing_names.size ();
	      missing_fields_str += it->first + (has_next ? ", " : "");
	      i++;
	    }

	  rust_error_at (pattern.get_locus (), ErrorCode::E0027,
			 "pattern does not mention fields %s",
			 missing_fields_str.c_str ());
	}
    }
}

void
TypeCheckPattern::visit (HIR::WildcardPattern &pattern)
{
  // wildcard patterns within the MatchArm's are simply just the same type as
  // the parent
  infered = parent->clone ();
  infered->set_ref (pattern.get_mappings ().get_hirid ());
}

void
TypeCheckPattern::visit (HIR::TuplePattern &pattern)
{
  std::unique_ptr<HIR::TuplePatternItems> items;
  switch (pattern.get_items ().get_item_type ())
    {
      case HIR::TuplePatternItems::ItemType::MULTIPLE: {
	auto &ref = static_cast<HIR::TuplePatternItemsMultiple &> (
	  pattern.get_items ());

	auto resolved_parent = parent->destructure ();
	if (resolved_parent->get_kind () != TyTy::TUPLE)
	  {
	    rust_error_at (pattern.get_locus (), "expected %s, found tuple",
			   parent->as_string ().c_str ());
	    break;
	  }

	const auto &patterns = ref.get_patterns ();
	size_t nitems_to_resolve = patterns.size ();

	TyTy::TupleType &par
	  = *static_cast<TyTy::TupleType *> (resolved_parent);
	if (patterns.size () != par.get_fields ().size ())
	  {
	    emit_pattern_size_error (pattern, par.get_fields ().size (),
				     patterns.size ());
	    nitems_to_resolve
	      = std::min (nitems_to_resolve, par.get_fields ().size ());
	  }

	std::vector<TyTy::TyVar> pattern_elems;
	for (size_t i = 0; i < nitems_to_resolve; i++)
	  {
	    auto &p = patterns[i];
	    TyTy::BaseType *par_type = par.get_field (i);

	    TyTy::BaseType *elem = TypeCheckPattern::Resolve (*p, par_type);
	    pattern_elems.push_back (TyTy::TyVar (elem->get_ref ()));
	  }
	infered = new TyTy::TupleType (pattern.get_mappings ().get_hirid (),
				       pattern.get_locus (), pattern_elems);
      }
      break;

      case HIR::TuplePatternItems::ItemType::RANGED: {
	// HIR::TuplePatternItemsRanged &ref
	//   = *static_cast<HIR::TuplePatternItemsRanged *> (
	//     pattern.get_items ().get ());
	// TODO
	rust_unreachable ();
      }
      break;
    }
}

void
TypeCheckPattern::visit (HIR::LiteralPattern &pattern)
{
  infered = resolve_literal (pattern.get_mappings (), pattern.get_literal (),
			     pattern.get_locus ());
}

void
TypeCheckPattern::visit (HIR::RangePattern &pattern)
{
  // Resolve the upper and lower bounds, and ensure they are compatible types
  TyTy::BaseType *upper = nullptr, *lower = nullptr;

  upper = typecheck_range_pattern_bound (pattern.get_upper_bound (),
					 pattern.get_mappings (),
					 pattern.get_locus ());

  lower = typecheck_range_pattern_bound (pattern.get_lower_bound (),
					 pattern.get_mappings (),
					 pattern.get_locus ());

  infered = unify_site (pattern.get_mappings ().get_hirid (),
			TyTy::TyWithLocation (upper),
			TyTy::TyWithLocation (lower), pattern.get_locus ());
}

void
TypeCheckPattern::visit (HIR::IdentifierPattern &pattern)
{
  if (!pattern.get_is_ref ())
    {
      infered = parent;
      return;
    }

  infered = new TyTy::ReferenceType (pattern.get_mappings ().get_hirid (),
				     TyTy::TyVar (parent->get_ref ()),
				     pattern.is_mut () ? Mutability::Mut
						       : Mutability::Imm);
}

void
TypeCheckPattern::visit (HIR::QualifiedPathInExpression &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "type checking qualified path patterns not supported");
}

void
TypeCheckPattern::visit (HIR::ReferencePattern &pattern)
{
  if (parent->get_kind () != TyTy::TypeKind::REF)
    {
      rust_error_at (pattern.get_locus (), "expected %s, found reference",
		     parent->as_string ().c_str ());
      return;
    }

  auto &ref_ty_ty = static_cast<TyTy::ReferenceType &> (*parent);
  TyTy::BaseType *infered_base
    = TypeCheckPattern::Resolve (pattern.get_referenced_pattern (),
				 ref_ty_ty.get_base ());
  infered = new TyTy::ReferenceType (pattern.get_mappings ().get_hirid (),
				     TyTy::TyVar (infered_base->get_ref ()),
				     pattern.is_mut () ? Mutability::Mut
						       : Mutability::Imm);
}

void
TypeCheckPattern::visit (HIR::SlicePattern &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "type checking qualified path patterns not supported");
}

void
TypeCheckPattern::emit_pattern_size_error (const HIR::Pattern &pattern,
					   size_t expected_field_count,
					   size_t got_field_count)
{
  rich_location r (line_table, pattern.get_locus ());
  r.add_range (mappings.lookup_location (parent->get_ref ()));
  rust_error_at (r,
		 "expected a tuple with %lu %s, found one "
		 "with %lu %s",
		 (unsigned long) expected_field_count,
		 expected_field_count == 1 ? "element" : "elements",
		 (unsigned long) got_field_count,
		 got_field_count == 1 ? "element" : "elements");
}

TyTy::BaseType *
TypeCheckPattern::typecheck_range_pattern_bound (
  Rust::HIR::RangePatternBound &bound, Analysis::NodeMapping mappings,
  location_t locus)
{
  TyTy::BaseType *resolved_bound = nullptr;
  switch (bound.get_bound_type ())
    {
      case HIR::RangePatternBound::RangePatternBoundType::LITERAL: {
	auto &ref = static_cast<HIR::RangePatternBoundLiteral &> (bound);

	HIR::Literal lit = ref.get_literal ();

	resolved_bound = resolve_literal (mappings, lit, locus);
      }
      break;

      case HIR::RangePatternBound::RangePatternBoundType::PATH: {
	auto &ref = static_cast<HIR::RangePatternBoundPath &> (bound);

	resolved_bound = TypeCheckExpr::Resolve (ref.get_path ());
      }
      break;

      case HIR::RangePatternBound::RangePatternBoundType::QUALPATH: {
	auto &ref = static_cast<HIR::RangePatternBoundQualPath &> (bound);

	resolved_bound = TypeCheckExpr::Resolve (ref.get_qualified_path ());
      }
      break;
    }

  return resolved_bound;
}

void
TypeCheckPattern::visit (HIR::AltPattern &pattern)
{
  const auto &alts = pattern.get_alts ();

  // lub - taken from TypeCheckExpr::visit(ArrayExpr)
  std::vector<TyTy::BaseType *> types;
  for (auto &alt_pattern : alts)
    {
      types.push_back (TypeCheckPattern::Resolve (*alt_pattern, parent));
    }

  TyTy::BaseType *alt_pattern_type
    = TyTy::TyVar::get_implicit_infer_var (pattern.get_locus ()).get_tyty ();

  for (auto &type : types)
    {
      alt_pattern_type
	= unify_site (pattern.get_mappings ().get_hirid (),
		      TyTy::TyWithLocation (alt_pattern_type),
		      TyTy::TyWithLocation (type, type->get_locus ()),
		      pattern.get_locus ());
    }

  infered = alt_pattern_type;
}

TyTy::BaseType *
ClosureParamInfer::Resolve (HIR::Pattern &pattern)
{
  ClosureParamInfer resolver;
  pattern.accept_vis (resolver);

  if (resolver.infered->get_kind () != TyTy::TypeKind::ERROR)
    {
      resolver.context->insert_implicit_type (resolver.infered->get_ref (),
					      resolver.infered);
      resolver.mappings.insert_location (resolver.infered->get_ref (),
					 pattern.get_locus ());
    }
  return resolver.infered;
}

ClosureParamInfer::ClosureParamInfer ()
  : TypeCheckBase (), infered (new TyTy::ErrorType (0))
{}

void
ClosureParamInfer::visit (HIR::WildcardPattern &pattern)
{
  HirId id = pattern.get_mappings ().get_hirid ();
  infered = new TyTy::InferType (id, TyTy::InferType::InferTypeKind::GENERAL,
				 TyTy::InferType::TypeHint::Default (),
				 pattern.get_locus ());
}

void
ClosureParamInfer::visit (HIR::IdentifierPattern &pattern)
{
  HirId id = pattern.get_mappings ().get_hirid ();
  infered = new TyTy::InferType (id, TyTy::InferType::InferTypeKind::GENERAL,
				 TyTy::InferType::TypeHint::Default (),
				 pattern.get_locus ());
}

void
ClosureParamInfer::visit (HIR::ReferencePattern &pattern)
{
  TyTy::BaseType *element
    = ClosureParamInfer::Resolve (pattern.get_referenced_pattern ());

  HirId id = pattern.get_mappings ().get_hirid ();
  infered = new TyTy::ReferenceType (id, TyTy::TyVar (element->get_ref ()),
				     pattern.get_mutability ());
}

void
ClosureParamInfer::visit (HIR::PathInExpression &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "unable to infer this kind of parameter pattern");
}

void
ClosureParamInfer::visit (HIR::StructPattern &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "unable to infer this kind of parameter pattern");
}

void
ClosureParamInfer::visit (HIR::TupleStructPattern &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "unable to infer this kind of parameter pattern");
}

void
ClosureParamInfer::visit (HIR::TuplePattern &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "unable to infer this kind of parameter pattern");
}

void
ClosureParamInfer::visit (HIR::LiteralPattern &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "unable to infer this kind of parameter pattern");
}

void
ClosureParamInfer::visit (HIR::RangePattern &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "unable to infer this kind of parameter pattern");
}

void
ClosureParamInfer::visit (HIR::QualifiedPathInExpression &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "unable to infer this kind of parameter pattern");
}

void
ClosureParamInfer::visit (HIR::SlicePattern &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "unable to infer this kind of parameter pattern");
}

void
ClosureParamInfer::visit (HIR::AltPattern &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "unable to infer this kind of parameter pattern");
}

} // namespace Resolver
} // namespace Rust
