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

#ifndef RUST_BIR_PLACE_H
#define RUST_BIR_PLACE_H

#include "rust-mapping-common.h"
#include "rust-system.h"
#include "rust-tyty.h"
#include "rust-bir-free-region.h"

#include "rust-tyty-variance-analysis.h"
#include "polonius/rust-polonius-ffi.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace BIR {

/** A unique identifier for a place in the BIR. */
using PlaceId = uint32_t;

static constexpr PlaceId INVALID_PLACE = 0;
static constexpr PlaceId RETURN_VALUE_PLACE = 1;
static constexpr PlaceId FIRST_VARIABLE_PLACE = RETURN_VALUE_PLACE;

using Variance = TyTy::VarianceAnalysis::Variance;
using LoanId = uint32_t;

/**
 * Representation of lvalues and constants in BIR.
 * See bir bir design notes (in this directory) and the Polonius book.
 */
struct Place
{
  enum Kind
  {
    INVALID,
    VARIABLE,
    TEMPORARY,
    CONSTANT,
    FIELD,
    INDEX,
    DEREF,
  };

  Kind kind;
  uint32_t variable_or_field_index; // NodeId for VARIABLE
  /** Data for traversing paths in the PlaceDB. */
  struct Path
  {
    PlaceId parent = INVALID_PLACE;
    PlaceId first_child = INVALID_PLACE;
    PlaceId next_sibling = INVALID_PLACE;

    Path (PlaceId parent, PlaceId first_child, PlaceId next_sibling)
      : parent (parent), first_child (first_child), next_sibling (next_sibling)
    {}
    Path () = default;
  } path;
  /** Copy trait */
  bool is_copy;
  bool has_drop = false;
  TyTy::BaseType *tyty;
  FreeRegions regions{{}};
  std::vector<LoanId> borrowed_by{};

public:
  Place (Kind kind, uint32_t variable_or_field_index, const Path &path,
	 bool is_copy, TyTy::BaseType *tyty)
    : kind (kind), variable_or_field_index (variable_or_field_index),
      path (path), is_copy (is_copy), tyty (tyty)
  {}

  // Place can only be stored in PlaceDB and used via reference. Turn all
  // accidental copies into errors.
  Place (const Place &) = delete;
  Place (Place &&) = default;

public:
  WARN_UNUSED_RESULT bool is_lvalue () const
  {
    return kind == VARIABLE || is_path ();
  }

  WARN_UNUSED_RESULT bool is_rvalue () const { return kind == TEMPORARY; }

  bool is_constant () const { return kind == CONSTANT; }

  WARN_UNUSED_RESULT bool is_var () const
  {
    return kind == VARIABLE || kind == TEMPORARY;
  }

  WARN_UNUSED_RESULT bool is_path () const
  {
    return kind == FIELD || kind == INDEX || kind == DEREF;
  }

  WARN_UNUSED_RESULT TyTy::BaseType *get_fn_return_ty () const
  {
    switch (tyty->get_kind ())
      {
      case TyTy::FNPTR:
	return tyty->as<TyTy::FnPtr> ()->get_return_type ();
      case TyTy::FNDEF:
	return tyty->as<TyTy::FnType> ()->get_return_type ();
      default:
	rust_assert (false);
      }
  }

  WARN_UNUSED_RESULT bool is_indirect () const
  {
    // TODO: probably incomplete, check other projections
    switch (tyty->get_kind ())
      {
      case TyTy::REF:
      case TyTy::POINTER:
	return true;
      default:
	return false;
      }
  }

  WARN_UNUSED_RESULT bool should_be_moved () const
  {
    return kind == TEMPORARY || (!is_copy && kind != CONSTANT);
  }
};

using ScopeId = uint32_t;

static constexpr ScopeId INVALID_SCOPE = std::numeric_limits<ScopeId>::max ();
/** Arguments and return value are in the root scope. */
static constexpr ScopeId ROOT_SCOPE = 0;
/** Top-level local variables are in the top-level scope. */
static constexpr ScopeId TOP_LEVEL_SCOPE = 1;

struct Scope
{
  ScopeId parent = INVALID_SCOPE;
  std::vector<ScopeId> children;
  std::vector<PlaceId> locals;
};

struct Loan
{
  Mutability mutability;
  PlaceId place;
};

/** Allocated places and keeps track of paths. */
class PlaceDB
{
private:
  // Possible optimizations: separate variables to speedup lookup.
  std::vector<Place> places;
  std::unordered_map<TyTy::BaseType *, PlaceId> constants_lookup;
  std::vector<Scope> scopes;
  ScopeId current_scope = 0;

  std::vector<Loan> loans;

  Polonius::Origin next_free_region = 1;

public:
  PlaceDB ()
  {
    // Reserved index for invalid place.
    places.push_back ({Place::INVALID, 0, {}, false, nullptr});

    scopes.emplace_back (); // Root scope.
  }

  Place &operator[] (PlaceId id) { return places.at (id); }
  const Place &operator[] (PlaceId id) const { return places.at (id); }

  decltype (places)::const_iterator begin () const { return places.begin (); }
  decltype (places)::const_iterator end () const { return places.end (); }

  size_t size () const { return places.size (); }

  const std::vector<Loan> &get_loans () const { return loans; }

  ScopeId get_current_scope_id () const { return current_scope; }

  const std::vector<Scope> &get_scopes () const { return scopes; }

  const Scope &get_current_scope () const { return scopes[current_scope]; }

  const Scope &get_scope (ScopeId id) const { return scopes[id]; }

  FreeRegion get_next_free_region () { return next_free_region++; }

  FreeRegion peek_next_free_region () const { return next_free_region; }

  FreeRegion &expose_next_free_region () { return next_free_region; }

  ScopeId push_new_scope ()
  {
    ScopeId new_scope = scopes.size ();
    scopes.emplace_back ();
    scopes[new_scope].parent = current_scope;
    scopes[current_scope].children.push_back (new_scope);
    current_scope = new_scope;
    return new_scope;
  }

  ScopeId pop_scope ()
  {
    current_scope = scopes[current_scope].parent;
    return current_scope;
  }

  PlaceId add_place (Place &&place, PlaceId last_sibling = 0)
  {
    places.emplace_back (std::forward<Place &&> (place));
    PlaceId new_place = places.size () - 1;
    Place &new_place_ref = places[new_place]; // Intentional shadowing.
    if (last_sibling == 0)
      places[new_place_ref.path.parent].path.first_child = new_place;
    else
      places[last_sibling].path.next_sibling = new_place;

    if (new_place_ref.kind == Place::VARIABLE
	|| new_place_ref.kind == Place::TEMPORARY)
      scopes[current_scope].locals.push_back (new_place);

    auto variances = Resolver::TypeCheckContext::get ()
		       ->get_variance_analysis_ctx ()
		       .query_type_variances (new_place_ref.tyty);
    std::vector<Polonius::Origin> regions;
    for (size_t i = 0; i < variances.size (); i++)
      regions.push_back (next_free_region++);

    new_place_ref.regions.set_from (std::move (regions));

    return new_place;
  }

  PlaceId add_variable (NodeId id, TyTy::BaseType *tyty)
  {
    return add_place ({Place::VARIABLE, id, {}, is_type_copy (tyty), tyty}, 0);
  }

  WARN_UNUSED_RESULT PlaceId lookup_or_add_path (Place::Kind kind,
						 TyTy::BaseType *tyty,
						 PlaceId parent, size_t id = 0)
  {
    PlaceId current = 0;
    if (parent < places.size ())
      {
	current = places[parent].path.first_child;
	while (current != 0)
	  {
	    if (places[current].kind == kind
		&& places[current].variable_or_field_index == id)
	      {
		rust_assert (places[current].tyty->is_equal (*tyty));
		return current;
	      }
	    current = places[current].path.next_sibling;
	  }
      }
    return add_place ({kind, (uint32_t) id, Place::Path{parent, 0, 0},
		       is_type_copy (tyty), tyty},
		      current);
  }

  PlaceId add_temporary (TyTy::BaseType *tyty)
  {
    return add_place ({Place::TEMPORARY, 0, {}, is_type_copy (tyty), tyty}, 0);
  }

  PlaceId get_constant (TyTy::BaseType *tyty)
  {
    auto lookup = constants_lookup.find (tyty);
    if (lookup != constants_lookup.end ())
      return lookup->second;
    return add_place ({Place::CONSTANT, 0, {}, is_type_copy (tyty), tyty});
  }

  PlaceId lookup_variable (NodeId id)
  {
    PlaceId current = FIRST_VARIABLE_PLACE;

    while (current != places.size ())
      {
	if (places[current].kind == Place::VARIABLE
	    && places[current].variable_or_field_index == id)
	  return current;
	current++;
      }
    return INVALID_PLACE;
  }

  LoanId add_loan (Loan &&loan)
  {
    LoanId id = loans.size ();
    loans.push_back (std::forward<Loan &&> (loan));
    PlaceId borrowed_place = loans.rbegin ()->place;
    places[loans.rbegin ()->place].borrowed_by.push_back (id);
    if (places[borrowed_place].kind == Place::DEREF)
      {
	places[places[borrowed_place].path.parent].borrowed_by.push_back (id);
      }
    return id;
  }

  PlaceId get_var (PlaceId id) const
  {
    if (places[id].is_var ())
      return id;
    rust_assert (places[id].is_path ());
    PlaceId current = id;
    while (!places[current].is_var ())
      {
	current = places[current].path.parent;
      }
    return current;
  }

  void set_next_free_region (Polonius::Origin next_free_region)
  {
    this->next_free_region = next_free_region;
  }

  PlaceId lookup_or_add_variable (NodeId id, TyTy::BaseType *tyty)
  {
    auto lookup = lookup_variable (id);
    if (lookup != INVALID_PLACE)
      return lookup;

    add_place ({Place::VARIABLE, id, {}, is_type_copy (tyty), tyty});
    return places.size () - 1;
  };

  template <typename FN> void for_each_path_from_root (PlaceId var, FN fn) const
  {
    PlaceId current = var;
    current = places[current].path.first_child;
    while (current != INVALID_PLACE)
      {
	fn (current);
	for_each_path_from_root (current, fn);
	current = places[current].path.next_sibling;
      }
  }

  template <typename FN>
  void for_each_path_segment (PlaceId place_id, FN fn) const
  {
    PlaceId current = place_id;
    while (current != INVALID_PLACE)
      {
	fn (current);
	current = places[current].path.parent;
      }
  }

private:
  static bool is_type_copy (TyTy::BaseType *ty)
  {
    switch (ty->get_kind ())
      {
      case TyTy::REF:
	return ty->as<TyTy::ReferenceType> ()->mutability () == Mutability::Imm;
      case TyTy::POINTER:
      case TyTy::SLICE:
      case TyTy::BOOL:
      case TyTy::CHAR:
      case TyTy::INT:
      case TyTy::UINT:
      case TyTy::FLOAT:
      case TyTy::USIZE:
      case TyTy::ISIZE:
      case TyTy::FNPTR:
      case TyTy::FNDEF:
      case TyTy::NEVER:
	return true;
	case TyTy::TUPLE: {
	  auto &fields = ty->as<TyTy::TupleType> ()->get_fields ();
	  return std::all_of (fields.begin (), fields.end (),
			      [] (const TyTy::TyVar &field) {
				return is_type_copy (field.get_tyty ());
			      });
	}
	case TyTy::ARRAY: {
	  return is_type_copy (ty->as<TyTy::ArrayType> ()->get_element_type ());
	}
      case TyTy::INFER:
      case TyTy::PARAM:
      case TyTy::ERROR:
      case TyTy::STR:
      case TyTy::PLACEHOLDER:
	rust_unreachable ();
      case TyTy::ADT:	     // TODO: check trait
      case TyTy::PROJECTION: // TODO: DUNNO
      case TyTy::CLOSURE:    // TODO: DUNNO
      case TyTy::DYNAMIC:    // TODO: dunno
	return false;
      }
    rust_unreachable ();
  }

  /** Check whether given place is not out-of-scope. */
  WARN_UNUSED_RESULT bool is_in_scope (PlaceId place) const
  {
    for (ScopeId scope = current_scope; scope != INVALID_SCOPE;
	 scope = scopes[scope].parent)
      {
	auto &scope_ref = scopes[scope];
	if (std::find (scope_ref.locals.begin (), scope_ref.locals.end (),
		       place)
	    != scope_ref.locals.end ())
	  return true;
      }
    return false;
  }
};

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_PLACE_H
