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
struct PlaceId
{
  uint32_t value;
  // some overloads for comparision
  bool operator== (const PlaceId &rhs) const { return value == rhs.value; }
  bool operator!= (const PlaceId &rhs) const { return !(operator== (rhs)); }
  bool operator< (const PlaceId &rhs) const { return value < rhs.value; }
  bool operator> (const PlaceId &rhs) const { return value > rhs.value; }
  bool operator<= (const PlaceId &rhs) const { return !(operator> (rhs)); }
  bool operator>= (const PlaceId &rhs) const { return !(operator< (rhs)); }
};

static constexpr PlaceId INVALID_PLACE = {0};
static constexpr PlaceId RETURN_VALUE_PLACE = {1};
static constexpr PlaceId FIRST_VARIABLE_PLACE = RETURN_VALUE_PLACE;

using Variance = TyTy::VarianceAnalysis::Variance;

/** A unique identifier for a loan in the BIR. */
struct LoanId
{
  uint32_t value;
  // some overloads for comparision
  bool operator== (const LoanId &rhs) const { return value == rhs.value; }
  bool operator!= (const LoanId &rhs) const { return !(operator== (rhs)); }
  bool operator< (const LoanId &rhs) const { return value < rhs.value; }
  bool operator> (const LoanId &rhs) const { return value > rhs.value; }
  bool operator<= (const LoanId &rhs) const { return !(operator> (rhs)); }
  bool operator>= (const LoanId &rhs) const { return !(operator< (rhs)); }
};

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

struct ScopeId
{
  uint32_t value;
  ScopeId next_scope_id () const { return {value + 1}; }
  // some overloads for comparision
  bool operator== (const ScopeId &rhs) const { return value == rhs.value; }
  bool operator!= (const ScopeId &rhs) const { return !(operator== (rhs)); }
  bool operator< (const ScopeId &rhs) const { return value < rhs.value; }
  bool operator> (const ScopeId &rhs) const { return value > rhs.value; }
  bool operator<= (const ScopeId &rhs) const { return !(operator> (rhs)); }
  bool operator>= (const ScopeId &rhs) const { return !(operator< (rhs)); }
};

static constexpr ScopeId INVALID_SCOPE
  = {std::numeric_limits<uint32_t>::max ()};
/** Arguments and return value are in the root scope. */
static constexpr ScopeId ROOT_SCOPE = {0};
/** Top-level local variables are in the top-level scope. */
static constexpr ScopeId TOP_LEVEL_SCOPE = {1};

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
  location_t location;
};

// I is the index type, T is the contained type
template <typename I, typename T> class IndexVec
{
  std::vector<T> internal_vector;

public:
  IndexVec () = default;
  IndexVec (size_t size) { internal_vector.reserve (size); }

  T &at (I pid) { return internal_vector[pid.value]; }
  const T &at (I pid) const { return internal_vector[pid.value]; }
  T &operator[] (I pid) { return internal_vector[pid.value]; }
  const T &operator[] (I pid) const { return internal_vector[pid.value]; }

  void push_back (T &&param) { internal_vector.push_back (std::move (param)); }
  template <typename... Args> void emplace_back (Args &&... args)
  {
    internal_vector.emplace_back (std::forward<Args> (args)...);
  }

  size_t size () const { return internal_vector.size (); }

  std::vector<T> &get_vector () { return internal_vector; }
};

using Scopes = IndexVec<ScopeId, Scope>;
using Loans = IndexVec<LoanId, Loan>;
using Places = IndexVec<PlaceId, Place>;

/** Allocated places and keeps track of paths. */
class PlaceDB
{
private:
  // Possible optimizations: separate variables to speedup lookup.
  Places places;
  std::unordered_map<TyTy::BaseType *, PlaceId> constants_lookup;
  Scopes scopes;
  ScopeId current_scope = ROOT_SCOPE;

  Loans loans;

  FreeRegion next_free_region = {1};

public:
  PlaceDB ()
  {
    // Reserved index for invalid place.
    places.push_back ({Place::INVALID, 0, {}, false, nullptr});

    scopes.emplace_back (); // Root scope.
  }

  Place &operator[] (PlaceId id) { return places.at (id); }
  const Place &operator[] (PlaceId id) const { return places.at (id); }

  size_t size () const { return places.size (); }

  const Loans &get_loans () const { return loans; }
  const Loan &get_loan (LoanId loan_id) const { return loans.at (loan_id); }

  ScopeId get_current_scope_id () const { return current_scope; }

  const Scopes &get_scopes () const { return scopes; }

  const Scope &get_current_scope () const { return scopes[current_scope]; }

  const Scope &get_scope (ScopeId id) const { return scopes[id]; }

  FreeRegion get_next_free_region ()
  {
    ++next_free_region.value;
    return {next_free_region.value - 1};
  }

  FreeRegion peek_next_free_region () const { return next_free_region; }

  FreeRegion &expose_next_free_region () { return next_free_region; }

  ScopeId push_new_scope ()
  {
    ScopeId new_scope = {scopes.size ()};
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

  PlaceId add_place (Place &&place, PlaceId last_sibling = INVALID_PLACE)
  {
    places.emplace_back (std::forward<Place &&> (place));
    PlaceId new_place = {places.size () - 1};
    Place &new_place_ref = places[new_place]; // Intentional shadowing.
    if (last_sibling == INVALID_PLACE)
      places[new_place_ref.path.parent].path.first_child = new_place;
    else
      places[last_sibling].path.next_sibling = new_place;

    if (new_place_ref.kind == Place::VARIABLE
	|| new_place_ref.kind == Place::TEMPORARY)
      scopes[current_scope].locals.push_back (new_place);

    auto variances = Resolver::TypeCheckContext::get ()
		       ->get_variance_analysis_ctx ()
		       .query_type_variances (new_place_ref.tyty);
    FreeRegions regions;
    for (size_t i = 0; i < variances.size (); ++i)
      {
	regions.push_back (next_free_region);
	++next_free_region.value;
      }

    new_place_ref.regions = regions;

    return new_place;
  }

  PlaceId add_variable (NodeId id, TyTy::BaseType *tyty)
  {
    return add_place ({Place::VARIABLE, id, {}, is_type_copy (tyty), tyty},
		      INVALID_PLACE);
  }

  WARN_UNUSED_RESULT PlaceId lookup_or_add_path (Place::Kind kind,
						 TyTy::BaseType *tyty,
						 PlaceId parent, size_t id = 0)
  {
    PlaceId current = INVALID_PLACE;
    if (parent.value < places.size ())
      {
	current = places[parent].path.first_child;
	while (current != INVALID_PLACE)
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
    return add_place ({kind, (uint32_t) id,
		       Place::Path{parent, INVALID_PLACE, INVALID_PLACE},
		       is_type_copy (tyty), tyty},
		      current);
  }

  PlaceId add_temporary (TyTy::BaseType *tyty)
  {
    return add_place ({Place::TEMPORARY, 0, {}, is_type_copy (tyty), tyty},
		      INVALID_PLACE);
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

    while (current.value != places.size ())
      {
	if (places[current].kind == Place::VARIABLE
	    && places[current].variable_or_field_index == id)
	  return current;
	++current.value;
      }
    return INVALID_PLACE;
  }

  LoanId add_loan (Loan &&loan)
  {
    LoanId id = {loans.size ()};
    loans.push_back (std::forward<Loan &&> (loan));
    PlaceId borrowed_place = loans.get_vector ().rbegin ()->place;
    places[loans.get_vector ().rbegin ()->place].borrowed_by.push_back (id);
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
    this->next_free_region.value = next_free_region;
  }

  PlaceId lookup_or_add_variable (NodeId id, TyTy::BaseType *tyty)
  {
    auto lookup = lookup_variable (id);
    if (lookup != INVALID_PLACE)
      return lookup;

    add_place ({Place::VARIABLE, id, {}, is_type_copy (tyty), tyty});
    return {places.size () - 1};
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
      case TyTy::OPAQUE:
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
