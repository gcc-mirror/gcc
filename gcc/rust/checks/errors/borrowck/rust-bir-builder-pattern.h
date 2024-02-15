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

#ifndef RUST_BIR_BUILDER_PATTERN_H
#define RUST_BIR_BUILDER_PATTERN_H

#include "rust-bir-builder-internal.h"

namespace Rust {
namespace BIR {

/**
 * Compiles binding of values into newly created variables.
 * Used in let, match arm, and function parameter patterns.
 */
class PatternBindingBuilder : protected AbstractBuilder,
			      public HIR::HIRPatternVisitor
{
  /** Value of initialization expression. */
  PlaceId init;
  /** This is where lifetime annotations are stored. */
  tl::optional<HIR::Type *> type;

  /** Emulates recursive stack saving and restoring inside a visitor. */
  class SavedState
  {
    PatternBindingBuilder *builder;

  public:
    const PlaceId init;
    const tl::optional<HIR::Type *> type;

  public:
    explicit SavedState (PatternBindingBuilder *builder)
      : builder (builder), init (builder->init), type (builder->type)
    {}

    ~SavedState ()
    {
      builder->init = init;
      builder->type = type;
    }
  };

public:
  PatternBindingBuilder (BuilderContext &ctx, PlaceId init, HIR::Type *type)
    : AbstractBuilder (ctx), init (init), type (optional_from_ptr (type))
  {}

  void go (HIR::Pattern &pattern) { pattern.accept_vis (*this); }

  void visit_identifier (const Analysis::NodeMapping &node, bool is_ref,
			 bool is_mut = false)
  {
    if (is_ref)
      {
	translated = declare_variable (
	  node, new TyTy::ReferenceType (node.get_hirid (),
					 TyTy::TyVar (node.get_hirid ()),
					 (is_mut) ? Mutability::Mut
						  : Mutability::Imm));
	push_assignment (translated, new BorrowExpr (init));
      }
    else
      {
	translated = declare_variable (node);
	push_assignment (translated, init);
      }
    auto &init_place = ctx.place_db[init];
    auto &translated_place = ctx.place_db[translated];
    if (init_place.tyty->get_kind () == TyTy::REF)
      {
	init_place.lifetime = ctx.lookup_lifetime (type.map ([] (HIR::Type *t) {
	  return static_cast<HIR::ReferenceType *> (t)->get_lifetime ();
	}));
	translated_place.lifetime = init_place.lifetime;
      }
  }

  void visit (HIR::IdentifierPattern &pattern) override
  {
    // Top-level identifiers are resolved directly to avoid useless temporary
    // (for cleaner BIR).
    visit_identifier (pattern.get_mappings (), pattern.get_is_ref (),
		      pattern.is_mut ());
  }

  void visit (HIR::ReferencePattern &pattern) override
  {
    SavedState saved (this);

    auto ref_type = type.map (
      [] (HIR::Type *t) { return static_cast<HIR::ReferenceType *> (t); });

    type = ref_type.map (
      [] (HIR::ReferenceType *r) { return r->get_base_type ().get (); });
    init = ctx.place_db.lookup_or_add_path (Place::DEREF, lookup_type (pattern),
					    saved.init);
    ctx.place_db[init].lifetime
      = ctx.lookup_lifetime (ref_type.map (&HIR::ReferenceType::get_lifetime));
    pattern.get_referenced_pattern ()->accept_vis (*this);
  }

  void visit (HIR::SlicePattern &pattern) override
  {
    SavedState saved (this);

    type = type.map ([] (HIR::Type *t) {
      return static_cast<HIR::SliceType *> (t)->get_element_type ().get ();
    });
    // All indexes are supposed to point to the same place for borrow-checking.
    init = ctx.place_db.lookup_or_add_path (Place::INDEX, lookup_type (pattern),
					    saved.init);
    for (auto &item : pattern.get_items ())
      {
	item->accept_vis (*this);
      }
  }

  void visit (HIR::AltPattern &pattern) override
  {
    rust_sorry_at (pattern.get_locus (),
		   "borrow-checking of alt patterns is not yet implemented");
  }

  void visit (HIR::StructPattern &pattern) override
  {
    SavedState saved (this);

    auto tyty = ctx.place_db[init].tyty;
    rust_assert (tyty->get_kind () == TyTy::ADT);
    auto adt_ty = static_cast<TyTy::ADTType *> (tyty);
    rust_assert (adt_ty->is_struct_struct ());
    auto struct_ty = adt_ty->get_variants ().at (0);

    auto struct_type = type.map ([] (HIR::Type *t) {
      return static_cast<HIR::TypePath *> (t)->get_final_segment ().get ();
    });
    for (auto &field :
	 pattern.get_struct_pattern_elems ().get_struct_pattern_fields ())
      {
	switch (field->get_item_type ())
	  {
	    case HIR::StructPatternField::TUPLE_PAT: {
	      auto tuple
		= static_cast<HIR::StructPatternFieldTuplePat *> (field.get ());
	      init = ctx.place_db.lookup_or_add_path (
		Place::FIELD, lookup_type (*tuple->get_tuple_pattern ()),
		saved.init, tuple->get_index ());
	      tuple->get_tuple_pattern ()->accept_vis (*this);
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
	      init
		= ctx.place_db.lookup_or_add_path (Place::FIELD,
						   field_ty->get_field_type (),
						   saved.init, field_index);
	      ident_field->get_pattern ()->accept_vis (*this);
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
	      init
		= ctx.place_db.lookup_or_add_path (Place::FIELD,
						   field_ty->get_field_type (),
						   saved.init, field_index);
	      visit_identifier (ident_field->get_mappings (),
				ident_field->get_has_ref (),
				ident_field->is_mut ());
	      break;
	    }
	  }
      }
  }

  void visit_tuple_fields (std::vector<std::unique_ptr<HIR::Pattern>> &fields,
			   SavedState &saved, size_t &index)
  {
    for (auto &item : fields)
      {
	type = saved.type.map ([&] (HIR::Type *t) {
	  return static_cast<HIR::TupleType *> (t)
	    ->get_elems ()
	    .at (index)
	    .get ();
	});
	init
	  = ctx.place_db.lookup_or_add_path (Place::FIELD, lookup_type (*item),
					     saved.init, index);
	item->accept_vis (*this);
	index++;
      }
  }
  void visit (HIR::TuplePattern &pattern) override
  {
    SavedState saved (this);

    size_t index = 0;
    switch (pattern.get_items ()->get_item_type ())
      {
	case HIR::TuplePatternItems::MULTIPLE: {
	  auto &items = static_cast<HIR::TuplePatternItemsMultiple &> (
	    *pattern.get_items ());
	  visit_tuple_fields (items.get_patterns (), saved, index);
	  break;
	}
	case HIR::TuplePatternItems::RANGED: {
	  auto &items = static_cast<HIR::TuplePatternItemsRanged &> (
	    *pattern.get_items ());

	  auto tyty = ctx.place_db[init].tyty;
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
  void visit (HIR::TupleStructPattern &pattern) override
  {
    SavedState saved (this);

    size_t index = 0;
    switch (pattern.get_items ()->get_item_type ())
      {
	case HIR::TupleStructItems::RANGED: {
	  auto &items
	    = static_cast<HIR::TupleStructItemsRange &> (*pattern.get_items ());

	  auto tyty = ctx.place_db[init].tyty;
	  rust_assert (tyty->get_kind () == TyTy::ADT);
	  auto adt_ty = static_cast<TyTy::ADTType *> (tyty);
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
	  auto &items = static_cast<HIR::TupleStructItemsNoRange &> (
	    *pattern.get_items ());
	  visit_tuple_fields (items.get_patterns (), saved, index);
	  break;
	}
      }
  }
  void visit (HIR::WildcardPattern &pattern) override {}

  // Unused for binding.
  void visit (HIR::LiteralPattern &pattern) override {}
  void visit (HIR::PathInExpression &expression) override {}
  void visit (HIR::QualifiedPathInExpression &expression) override {}
  void visit (HIR::RangePattern &pattern) override {}

private:
  template <typename T> tl::optional<T> *get_type ()
  {
    return static_cast<T *> (type);
  }
};
} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_BUILDER_PATTERN_H
