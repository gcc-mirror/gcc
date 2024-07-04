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

#ifndef RUST_BIR_PLACE_H
#define RUST_BIR_PLACE_H

#include <limits>
#include "rust-mapping-common.h"
#include "rust-system.h"
#include "rust-tyty.h"

namespace Rust {
namespace BIR {

/** A unique identifier for a place in the BIR. */
using PlaceId = uint32_t;

static constexpr PlaceId INVALID_PLACE = 0;
static constexpr PlaceId RETURN_VALUE_PLACE = 1;
static constexpr PlaceId FIRST_VARIABLE_PLACE = RETURN_VALUE_PLACE;

/**
 * A unique identifier for a lifetime in the BIR. Only to be used INTERNALLY.
 */
using LifetimeID = uint32_t;

constexpr LifetimeID INVALID_LIFETIME_ID = 0;
constexpr LifetimeID STATIC_LIFETIME_ID = 1;
constexpr LifetimeID FIRST_NORMAL_LIFETIME_ID = 2;

/** Representation of lifetimes in BIR. */
struct Lifetime
{
  LifetimeID id = INVALID_LIFETIME_ID;

  constexpr Lifetime (LifetimeID id) : id (id) {}
  constexpr Lifetime (const Lifetime &) = default;
  WARN_UNUSED_RESULT bool has_lifetime () const
  {
    return id != INVALID_LIFETIME_ID;
  }
  LifetimeID operator() () const { return id; }
};
constexpr Lifetime NO_LIFETIME = {INVALID_LIFETIME_ID};
constexpr Lifetime STATIC_LIFETIME = {STATIC_LIFETIME_ID};

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
  /** This place can be moved from safety. */
  bool is_rvalue;
  Lifetime lifetime;
  TyTy::BaseType *tyty;

  Place (Kind kind, uint32_t variable_or_field_index, const Path &path,
	 bool is_copy, bool is_rvalue, const Lifetime &lifetime,
	 TyTy::BaseType *tyty)
    : kind (kind), variable_or_field_index (variable_or_field_index),
      path (path), is_copy (is_copy), is_rvalue (is_rvalue),
      lifetime (lifetime), tyty (tyty)
  {}
};

/** Allocated places and keeps track of paths. */
class PlaceDB
{
  // Possible optimizations: separate variables to speedup lookup.
  std::vector<Place> places;
  std::unordered_map<TyTy::BaseType *, PlaceId> constants_lookup;

public:
  PlaceDB ()
  {
    // Reserved index for invalid place.
    places.push_back (
      {Place::INVALID, 0, {}, false, false, NO_LIFETIME, nullptr});
  }

  Place &operator[] (PlaceId id) { return places.at (id); }
  const Place &operator[] (PlaceId id) const { return places.at (id); }

  size_t size () const { return places.size (); }

  PlaceId add_place (Place place, PlaceId last_sibling = 0)
  {
    places.push_back (place);
    PlaceId new_place = places.size () - 1;
    if (last_sibling == 0)
      {
	places[place.path.parent].path.first_child = new_place;
      }
    else
      {
	places[last_sibling].path.next_sibling = new_place;
      }
    return new_place;
  }

  PlaceId add_variable (NodeId id, TyTy::BaseType *tyty)
  {
    return add_place (
      {Place::VARIABLE, id, {}, is_type_copy (tyty), false, NO_LIFETIME, tyty},
      0);
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
    return add_place (
      {kind, id, {parent, 0, 0}, is_type_copy (tyty), false, NO_LIFETIME, tyty},
      current);
  }

  PlaceId add_temporary (TyTy::BaseType *tyty)
  {
    return add_place (
      {Place::TEMPORARY, 0, {}, is_type_copy (tyty), false, NO_LIFETIME, tyty},
      0);
  }

  PlaceId get_constant (TyTy::BaseType *tyty)
  {
    auto lookup = constants_lookup.find (tyty);
    if (lookup != constants_lookup.end ())
      return lookup->second;
    Lifetime lifetime
      = tyty->get_kind () == TyTy::REF ? STATIC_LIFETIME : NO_LIFETIME;
    Place place
      = {Place::CONSTANT, 0, {}, is_type_copy (tyty), false, lifetime, tyty};
    places.push_back (place);
    return places.size () - 1;
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
  };

  PlaceId lookup_or_add_variable (NodeId id, TyTy::BaseType *tyty)
  {
    auto lookup = lookup_variable (id);
    if (lookup != INVALID_PLACE)
      return lookup;
    add_place (
      {Place::VARIABLE, id, {}, is_type_copy (tyty), false, NO_LIFETIME, tyty});
    return places.size () - 1;
  };

  PlaceId into_rvalue (PlaceId place)
  {
    if (places[place].is_rvalue || places[place].kind == Place::CONSTANT
	|| places[place].tyty->get_kind () == TyTy::REF)
      return place;
    return add_place ({Place::TEMPORARY,
		       0,
		       {},
		       places[place].is_copy,
		       true,
		       NO_LIFETIME,
		       places[place].tyty});
  }

private:
  static bool is_type_copy (TyTy::BaseType *ty)
  {
    switch (ty->get_kind ())
      {
      case TyTy::REF:
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
};

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_PLACE_H
