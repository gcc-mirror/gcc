#include "rust-bir-builder-pattern.h"

namespace Rust {
namespace BIR {

void
PatternBindingBuilder::visit_identifier (const Analysis::NodeMapping &node,
					 bool is_ref, location_t location,
					 bool is_mut)
{
  if (is_ref)
    {
      translated = declare_variable (
	node,
	new TyTy::ReferenceType (node.get_hirid (),
				 TyTy::TyVar (node.get_hirid ()),
				 (is_mut) ? Mutability::Mut : Mutability::Imm));
    }
  else
    {
      translated = declare_variable (node);
    }

  if (init.has_value ())
    {
      push_assignment (translated, init.value (), location);
    }
}

void
PatternBindingBuilder::visit (HIR::IdentifierPattern &pattern)
{
  // Top-level identifiers are resolved directly to avoid useless temporary
  // (for cleaner BIR).
  visit_identifier (pattern.get_mappings (), pattern.get_is_ref (),
		    pattern.get_locus (), pattern.is_mut ());
}

void
PatternBindingBuilder::visit (HIR::ReferencePattern &pattern)
{
  SavedState saved (this);

  init = init.map ([&] (PlaceId id) {
    return ctx.place_db.lookup_or_add_path (Place::DEREF, lookup_type (pattern),
					    id);
  });

  type_annotation = type_annotation.map ([&] (TyTy::BaseType *ty) {
    return ty->as<TyTy::ReferenceType> ()->get_base ();
  });

  pattern.get_referenced_pattern ().accept_vis (*this);
}

void
PatternBindingBuilder::visit (HIR::SlicePattern &pattern)
{
  SavedState saved (this);

  // All indexes are supposed to point to the same place for borrow-checking.
  // init = ctx.place_db.lookup_or_add_path (Place::INDEX, lookup_type
  // (pattern), saved.init);
  init = init.map ([&] (PlaceId id) {
    return ctx.place_db.lookup_or_add_path (Place::INDEX, lookup_type (pattern),
					    id);
  });

  type_annotation = type_annotation.map ([&] (TyTy::BaseType *ty) {
    return ty->as<TyTy::SliceType> ()->get_element_type ();
  });

  // Regions are unchnaged.

  for (auto &item : pattern.get_items ())
    {
      item->accept_vis (*this);
    }
}

void
PatternBindingBuilder::visit (HIR::AltPattern &pattern)
{
  rust_sorry_at (pattern.get_locus (),
		 "borrow-checking of alt patterns is not yet implemented");
}

void
PatternBindingBuilder::visit (HIR::StructPattern &pattern)
{
  SavedState saved (this);

  auto tyty = ctx.place_db[init.value ()].tyty;
  rust_assert (tyty->get_kind () == TyTy::ADT);
  auto adt_ty = static_cast<TyTy::ADTType *> (tyty);
  rust_assert (adt_ty->is_struct_struct ());
  auto struct_ty = adt_ty->get_variants ().at (0);

  for (auto &field :
       pattern.get_struct_pattern_elems ().get_struct_pattern_fields ())
    {
      switch (field->get_item_type ())
	{
	  case HIR::StructPatternField::TUPLE_PAT: {
	    auto tuple
	      = static_cast<HIR::StructPatternFieldTuplePat *> (field.get ());

	    init = init.map ([&] (PlaceId id) {
	      return ctx.place_db.lookup_or_add_path (
		Place::FIELD, lookup_type (tuple->get_tuple_pattern ()), id,
		tuple->get_index ());
	    });

	    type_annotation = type_annotation.map ([&] (TyTy::BaseType *ty) {
	      return ty->as<TyTy::ADTType> ()
		->get_variants ()
		.at (0)
		->get_fields ()
		.at (tuple->get_index ())
		->get_field_type ();
	    });

	    tuple->get_tuple_pattern ().accept_vis (*this);
	    break;
	  }
	  case HIR::StructPatternField::IDENT_PAT: {
	    auto ident_field
	      = static_cast<HIR::StructPatternFieldIdentPat *> (field.get ());
	    TyTy::StructFieldType *field_ty = nullptr;
	    size_t field_index = 0;
	    auto ok = struct_ty->lookup_field (
	      ident_field->get_identifier ().as_string (), &field_ty,
	      &field_index);
	    rust_assert (ok);
	    init = ctx.place_db.lookup_or_add_path (Place::FIELD,
						    field_ty->get_field_type (),
						    saved.init.value (),
						    field_index);
	    ident_field->get_pattern ().accept_vis (*this);
	    break;
	  }
	  case HIR::StructPatternField::IDENT: {
	    auto ident_field
	      = static_cast<HIR::StructPatternFieldIdent *> (field.get ());
	    TyTy::StructFieldType *field_ty = nullptr;
	    size_t field_index = 0;
	    auto ok = struct_ty->lookup_field (
	      ident_field->get_identifier ().as_string (), &field_ty,
	      &field_index);
	    rust_assert (ok);
	    init = ctx.place_db.lookup_or_add_path (Place::FIELD,
						    field_ty->get_field_type (),
						    saved.init.value (),
						    field_index);
	    visit_identifier (ident_field->get_mappings (),
			      ident_field->get_has_ref (),
			      ident_field->get_locus (),
			      ident_field->is_mut ());
	    break;
	  }
	}
    }
}

void
PatternBindingBuilder::visit_tuple_fields (
  std::vector<std::unique_ptr<HIR::Pattern>> &fields, SavedState &saved,
  size_t &index)
{
  for (auto &item : fields)
    {
      auto type = lookup_type (*item);

      init = init.map ([&] (PlaceId id) {
	return ctx.place_db.lookup_or_add_path (Place::FIELD, type, id, index);
      });

      type_annotation = type_annotation.map ([&] (TyTy::BaseType *ty) {
	return ty->as<TyTy::TupleType> ()->get_fields ().at (index).get_tyty ();
      });

      regions = regions.map ([&] (FreeRegions regs) {
	return bind_regions (Resolver::TypeCheckContext::get ()
			       ->get_variance_analysis_ctx ()
			       .query_type_regions (type),
			     regs);
      });

      item->accept_vis (*this);
      index++;
    }
}

void
PatternBindingBuilder::visit (HIR::TuplePattern &pattern)
{
  SavedState saved (this);

  size_t index = 0;
  switch (pattern.get_items ().get_item_type ())
    {
      case HIR::TuplePatternItems::MULTIPLE: {
	auto &items = static_cast<HIR::TuplePatternItemsMultiple &> (
	  pattern.get_items ());
	visit_tuple_fields (items.get_patterns (), saved, index);
	break;
      }
      case HIR::TuplePatternItems::RANGED: {
	auto &items
	  = static_cast<HIR::TuplePatternItemsRanged &> (pattern.get_items ());

	auto tyty = ctx.place_db[init.value ()].tyty;
	rust_assert (tyty->get_kind () == TyTy::TUPLE);

	auto skipped = (static_cast<TyTy::TupleType *> (tyty))->num_fields ()
		       - items.get_lower_patterns ().size ()
		       - items.get_upper_patterns ().size ();

	visit_tuple_fields (items.get_lower_patterns (), saved, index);
	index += skipped;
	visit_tuple_fields (items.get_upper_patterns (), saved, index);
	break;
      }
    }
  init = saved.init;
}

void
PatternBindingBuilder::visit (HIR::TupleStructPattern &pattern)
{
  SavedState saved (this);

  type_annotation = tl::nullopt;

  auto type = lookup_type (pattern);

  regions = regions.map ([&] (FreeRegions regs) {
    return bind_regions (Resolver::TypeCheckContext::get ()
			   ->get_variance_analysis_ctx ()
			   .query_type_regions (type),
			 regs);
  });

  size_t index = 0;
  switch (pattern.get_items ().get_item_type ())
    {
      case HIR::TupleStructItems::RANGED: {
	auto &items
	  = static_cast<HIR::TupleStructItemsRange &> (pattern.get_items ());

	rust_assert (type->get_kind () == TyTy::ADT);
	auto adt_ty = static_cast<TyTy::ADTType *> (type);
	rust_assert (adt_ty->is_tuple_struct ());

	auto skipped = adt_ty->get_variants ().at (0)->get_fields ().size ()
		       - items.get_lower_patterns ().size ()
		       - items.get_upper_patterns ().size ();

	visit_tuple_fields (items.get_lower_patterns (), saved, index);
	index += skipped;
	visit_tuple_fields (items.get_upper_patterns (), saved, index);
	break;
      }
      case HIR::TupleStructItems::MULTIPLE: {
	auto &items
	  = static_cast<HIR::TupleStructItemsNoRange &> (pattern.get_items ());
	visit_tuple_fields (items.get_patterns (), saved, index);
	break;
      }
    }
}
} // namespace BIR
} // namespace Rust
