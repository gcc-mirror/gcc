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

#ifndef RUST_HIR_TYPE_H
#define RUST_HIR_TYPE_H

#include "rust-common.h"
#include "rust-hir.h"
#include "rust-hir-path.h"

namespace Rust {
namespace HIR {
// definitions moved to rust-ast.h
class TypeParamBound;
class Lifetime;

// A trait bound
class TraitBound : public TypeParamBound
{
  bool in_parens;
  BoundPolarity polarity;
  std::vector<LifetimeParam> for_lifetimes;
  TypePath type_path;
  location_t locus;

  Analysis::NodeMapping mappings;

public:
  // Returns whether trait bound has "for" lifetimes
  bool has_for_lifetimes () const { return !for_lifetimes.empty (); }

  TraitBound (Analysis::NodeMapping mapping, TypePath type_path,
	      location_t locus, bool in_parens = false,
	      BoundPolarity polarity = BoundPolarity::RegularBound,
	      std::vector<LifetimeParam> for_lifetimes
	      = std::vector<LifetimeParam> ())
    : in_parens (in_parens), polarity (polarity),
      for_lifetimes (std::move (for_lifetimes)),
      type_path (std::move (type_path)), locus (locus), mappings (mapping)
  {}

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;

  Analysis::NodeMapping get_mappings () const override final
  {
    return mappings;
  }

  std::vector<LifetimeParam> &get_for_lifetimes () { return for_lifetimes; }
  bool get_in_parens () { return in_parens; }
  BoundPolarity get_polarity () { return polarity; }

  BoundType get_bound_type () const final override { return TRAITBOUND; }

  TypePath &get_path () { return type_path; }

  const TypePath &get_path () const { return type_path; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TraitBound *clone_type_param_bound_impl () const override
  {
    return new TraitBound (*this);
  }
};

// definition moved to rust-ast.h
class TypeNoBounds;

// An impl trait? Poor reference material here.
class ImplTraitType : public Type
{
  // TypeParamBounds type_param_bounds;
  // inlined form
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ImplTraitType *clone_type_impl () const override
  {
    return new ImplTraitType (*this);
  }

public:
  ImplTraitType (Analysis::NodeMapping mappings,
		 std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
		 location_t locus)
    : Type (mappings, locus), type_param_bounds (std::move (type_param_bounds))
  {}

  // copy constructor with vector clone
  ImplTraitType (ImplTraitType const &other)
    : Type (other.mappings, other.locus)
  {
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());
  }

  // overloaded assignment operator to clone
  ImplTraitType &operator= (ImplTraitType const &other)
  {
    locus = other.locus;
    mappings = other.mappings;

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    return *this;
  }

  // move constructors
  ImplTraitType (ImplTraitType &&other) = default;
  ImplTraitType &operator= (ImplTraitType &&other) = default;

  std::string as_string () const override;
  std::vector<std::unique_ptr<TypeParamBound>> &get_type_param_bounds ()
  {
    return type_param_bounds;
  }
  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;
};

// An opaque value of another type that implements a set of traits
class TraitObjectType : public Type
{
  bool has_dyn;
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TraitObjectType *clone_type_impl () const override
  {
    return new TraitObjectType (*this);
  }

public:
  TraitObjectType (
    Analysis::NodeMapping mappings,
    std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
    location_t locus, bool is_dyn_dispatch)
    : Type (mappings, locus), has_dyn (is_dyn_dispatch),
      type_param_bounds (std::move (type_param_bounds))
  {}

  // copy constructor with vector clone
  TraitObjectType (TraitObjectType const &other)
    : Type (other.mappings, other.locus), has_dyn (other.has_dyn)
  {
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());
  }

  // overloaded assignment operator to clone
  TraitObjectType &operator= (TraitObjectType const &other)
  {
    mappings = other.mappings;
    has_dyn = other.has_dyn;
    locus = other.locus;
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    return *this;
  }

  // move constructors
  TraitObjectType (TraitObjectType &&other) = default;
  TraitObjectType &operator= (TraitObjectType &&other) = default;

  std::string as_string () const override;
  bool get_has_dyn () { return has_dyn; }
  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;

  std::vector<std::unique_ptr<TypeParamBound>> &get_type_param_bounds ()
  {
    return type_param_bounds;
  }

  const std::vector<std::unique_ptr<TypeParamBound>> &
  get_type_param_bounds () const
  {
    return type_param_bounds;
  }
};

// A type with parentheses around it, used to avoid ambiguity.
class ParenthesisedType : public TypeNoBounds
{
  std::unique_ptr<Type> type_in_parens;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ParenthesisedType *clone_type_impl () const override
  {
    return new ParenthesisedType (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ParenthesisedType *clone_type_no_bounds_impl () const override
  {
    return new ParenthesisedType (*this);
  }

public:
  // Constructor uses Type pointer for polymorphism
  ParenthesisedType (Analysis::NodeMapping mappings,
		     std::unique_ptr<Type> type_inside_parens, location_t locus)
    : TypeNoBounds (mappings, locus),
      type_in_parens (std::move (type_inside_parens))
  {}

  /* Copy constructor uses custom deep copy method for type to preserve
   * polymorphism */
  ParenthesisedType (ParenthesisedType const &other)
    : TypeNoBounds (other.mappings, other.locus),
      type_in_parens (other.type_in_parens->clone_type ())
  {}

  // overload assignment operator to use custom clone method
  ParenthesisedType &operator= (ParenthesisedType const &other)
  {
    mappings = other.mappings;
    type_in_parens = other.type_in_parens->clone_type ();
    locus = other.locus;
    return *this;
  }

  // default move semantics
  ParenthesisedType (ParenthesisedType &&other) = default;
  ParenthesisedType &operator= (ParenthesisedType &&other) = default;

  std::string as_string () const override
  {
    return "(" + type_in_parens->as_string () + ")";
  }

  // Creates a trait bound (clone of this one's trait bound) - HACK
  TraitBound *to_trait_bound (bool in_parens ATTRIBUTE_UNUSED) const override
  {
    /* NOTE: obviously it is unknown whether the internal type is a trait bound
     * due to polymorphism, so just let the internal type handle it. As
     * parenthesised type, it must be in parentheses. */
    return type_in_parens->to_trait_bound (true);
  }
  std::unique_ptr<Type> &get_type_in_parens () { return type_in_parens; }
  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;
};

// Impl trait with a single bound? Poor reference material here.
class ImplTraitTypeOneBound : public TypeNoBounds
{
  TraitBound trait_bound;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ImplTraitTypeOneBound *clone_type_impl () const override
  {
    return new ImplTraitTypeOneBound (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ImplTraitTypeOneBound *clone_type_no_bounds_impl () const override
  {
    return new ImplTraitTypeOneBound (*this);
  }

public:
  ImplTraitTypeOneBound (Analysis::NodeMapping mappings, TraitBound trait_bound,
			 location_t locus)
    : TypeNoBounds (mappings, locus), trait_bound (std::move (trait_bound))
  {}

  std::string as_string () const override;
  TraitBound &get_trait_bound () { return trait_bound; }
  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;
};

class TypePath; // definition moved to "rust-path.h"

/* A type consisting of the "product" of others (the tuple's elements) in a
 * specific order */
class TupleType : public TypeNoBounds
{
  std::vector<std::unique_ptr<Type>> elems;

public:
  // Returns whether the tuple type is the unit type, i.e. has no elements.
  bool is_unit_type () const { return elems.empty (); }

  TupleType (Analysis::NodeMapping mappings,
	     std::vector<std::unique_ptr<Type>> elems, location_t locus)
    : TypeNoBounds (mappings, locus), elems (std::move (elems))
  {}

  // copy constructor with vector clone
  TupleType (TupleType const &other)
    : TypeNoBounds (other.mappings, other.locus)
  {
    mappings = other.mappings;
    elems.reserve (other.elems.size ());
    for (const auto &e : other.elems)
      elems.push_back (e->clone_type ());
  }

  // overloaded assignment operator to clone
  TupleType &operator= (TupleType const &other)
  {
    locus = other.locus;

    elems.reserve (other.elems.size ());
    for (const auto &e : other.elems)
      elems.push_back (e->clone_type ());

    return *this;
  }

  // move constructors
  TupleType (TupleType &&other) = default;
  TupleType &operator= (TupleType &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;

  std::vector<std::unique_ptr<Type>> &get_elems () { return elems; }
  const std::vector<std::unique_ptr<Type>> &get_elems () const { return elems; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleType *clone_type_impl () const override { return new TupleType (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleType *clone_type_no_bounds_impl () const override
  {
    return new TupleType (*this);
  }
};

/* A type with no values, representing the result of computations that never
 * complete. Expressions of NeverType can be coerced into any other types.
 * Represented as "!". */
class NeverType : public TypeNoBounds
{
protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  NeverType *clone_type_impl () const override { return new NeverType (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  NeverType *clone_type_no_bounds_impl () const override
  {
    return new NeverType (*this);
  }

public:
  NeverType (Analysis::NodeMapping mappings, location_t locus)
    : TypeNoBounds (mappings, locus)
  {}

  std::string as_string () const override { return "! (never type)"; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;
};

// A type consisting of a pointer without safety or liveness guarantees
class RawPointerType : public TypeNoBounds
{
private:
  Mutability mut;
  std::unique_ptr<Type> type;

public:
  // Constructor requires pointer for polymorphism reasons
  RawPointerType (Analysis::NodeMapping mappings, Mutability mut,
		  std::unique_ptr<Type> type, location_t locus)
    : TypeNoBounds (mappings, locus), mut (mut), type (std::move (type))
  {}

  // Copy constructor calls custom polymorphic clone function
  RawPointerType (RawPointerType const &other)
    : TypeNoBounds (other.mappings, other.locus), mut (other.mut),
      type (other.type->clone_type ())
  {}

  // overload assignment operator to use custom clone method
  RawPointerType &operator= (RawPointerType const &other)
  {
    mappings = other.mappings;
    mut = other.mut;
    type = other.type->clone_type ();
    locus = other.locus;
    return *this;
  }

  // default move semantics
  RawPointerType (RawPointerType &&other) = default;
  RawPointerType &operator= (RawPointerType &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;

  std::unique_ptr<Type> &get_type () { return type; }

  Mutability get_mut () const { return mut; }

  bool is_mut () const { return mut == Mutability::Mut; }

  bool is_const () const { return mut == Mutability::Imm; }

  std::unique_ptr<Type> &get_base_type () { return type; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RawPointerType *clone_type_impl () const override
  {
    return new RawPointerType (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RawPointerType *clone_type_no_bounds_impl () const override
  {
    return new RawPointerType (*this);
  }
};

// A type pointing to memory owned by another value
class ReferenceType : public TypeNoBounds
{
  // bool has_lifetime; // TODO: handle in lifetime or something?
  Lifetime lifetime;

  Mutability mut;
  std::unique_ptr<Type> type;

public:
  // Returns whether the reference is mutable or immutable.
  bool is_mut () const { return mut == Mutability::Mut; }

  // Returns whether the reference has a lifetime.
  bool has_lifetime () const { return !lifetime.is_error (); }

  // Constructor
  ReferenceType (Analysis::NodeMapping mappings, Mutability mut,
		 std::unique_ptr<Type> type_no_bounds, location_t locus,
		 Lifetime lifetime)
    : TypeNoBounds (mappings, locus), lifetime (std::move (lifetime)),
      mut (mut), type (std::move (type_no_bounds))
  {}

  // Copy constructor with custom clone method
  ReferenceType (ReferenceType const &other)
    : TypeNoBounds (other.mappings, other.locus), lifetime (other.lifetime),
      mut (other.mut), type (other.type->clone_type ())
  {}

  // Operator overload assignment operator to custom clone the unique pointer
  ReferenceType &operator= (ReferenceType const &other)
  {
    mappings = other.mappings;
    lifetime = other.lifetime;
    mut = other.mut;
    type = other.type->clone_type ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  ReferenceType (ReferenceType &&other) = default;
  ReferenceType &operator= (ReferenceType &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;

  Lifetime &get_lifetime () { return lifetime; }

  Mutability get_mut () const { return mut; }

  std::unique_ptr<Type> &get_base_type () { return type; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ReferenceType *clone_type_impl () const override
  {
    return new ReferenceType (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ReferenceType *clone_type_no_bounds_impl () const override
  {
    return new ReferenceType (*this);
  }
};

// A fixed-size sequence of elements of a specified type
class ArrayType : public TypeNoBounds
{
  std::unique_ptr<Type> elem_type;
  std::unique_ptr<Expr> size;

public:
  // Constructor requires pointers for polymorphism
  ArrayType (Analysis::NodeMapping mappings, std::unique_ptr<Type> type,
	     std::unique_ptr<Expr> array_size, location_t locus)
    : TypeNoBounds (mappings, locus), elem_type (std::move (type)),
      size (std::move (array_size))
  {}

  // Copy constructor requires deep copies of both unique pointers
  ArrayType (ArrayType const &other)
    : TypeNoBounds (other.mappings, other.locus),
      elem_type (other.elem_type->clone_type ()),
      size (other.size->clone_expr ())
  {}

  // Overload assignment operator to deep copy pointers
  ArrayType &operator= (ArrayType const &other)
  {
    mappings = other.mappings;
    elem_type = other.elem_type->clone_type ();
    size = other.size->clone_expr ();
    locus = other.locus;
    return *this;
  }

  // move constructors
  ArrayType (ArrayType &&other) = default;
  ArrayType &operator= (ArrayType &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;

  std::unique_ptr<Type> &get_element_type () { return elem_type; }

  std::unique_ptr<Expr> &get_size_expr () { return size; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ArrayType *clone_type_impl () const override { return new ArrayType (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ArrayType *clone_type_no_bounds_impl () const override
  {
    return new ArrayType (*this);
  }
};

/* A dynamically-sized type representing a "view" into a sequence of elements of
 * a type */
class SliceType : public TypeNoBounds
{
  std::unique_ptr<Type> elem_type;

public:
  // Constructor requires pointer for polymorphism
  SliceType (Analysis::NodeMapping mappings, std::unique_ptr<Type> type,
	     location_t locus)
    : TypeNoBounds (mappings, locus), elem_type (std::move (type))
  {}

  // Copy constructor requires deep copy of Type smart pointer
  SliceType (SliceType const &other)
    : TypeNoBounds (other.mappings, other.locus),
      elem_type (other.elem_type->clone_type ())
  {}

  // Overload assignment operator to deep copy
  SliceType &operator= (SliceType const &other)
  {
    mappings = other.mappings;
    elem_type = other.elem_type->clone_type ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  SliceType (SliceType &&other) = default;
  SliceType &operator= (SliceType &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;

  std::unique_ptr<Type> &get_element_type () { return elem_type; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  SliceType *clone_type_impl () const override { return new SliceType (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  SliceType *clone_type_no_bounds_impl () const override
  {
    return new SliceType (*this);
  }
};

/* Type used in generic arguments to explicitly request type inference (wildcard
 * pattern) */
class InferredType : public TypeNoBounds
{
  // e.g. Vec<_> = whatever
protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  InferredType *clone_type_impl () const override
  {
    return new InferredType (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  InferredType *clone_type_no_bounds_impl () const override
  {
    return new InferredType (*this);
  }

public:
  InferredType (Analysis::NodeMapping mappings, location_t locus)
    : TypeNoBounds (mappings, locus)
  {}

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;
};

class QualifiedPathInType; // definition moved to "rust-path.h"

// A possibly named param used in a BaseFunctionType
struct MaybeNamedParam
{
public:
  enum ParamKind
  {
    UNNAMED,
    IDENTIFIER,
    WILDCARD
  };

private:
  std::unique_ptr<Type> param_type;

  ParamKind param_kind;
  Identifier name; // technically, can be an identifier or '_'

  location_t locus;

public:
  MaybeNamedParam (Identifier name, ParamKind param_kind,
		   std::unique_ptr<Type> param_type, location_t locus)
    : param_type (std::move (param_type)), param_kind (param_kind),
      name (std::move (name)), locus (locus)
  {}

  // Copy constructor with clone
  MaybeNamedParam (MaybeNamedParam const &other)
    : param_type (other.param_type->clone_type ()),
      param_kind (other.param_kind), name (other.name), locus (other.locus)
  {}

  ~MaybeNamedParam () = default;

  // Overloaded assignment operator with clone
  MaybeNamedParam &operator= (MaybeNamedParam const &other)
  {
    name = other.name;
    param_kind = other.param_kind;
    param_type = other.param_type->clone_type ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  MaybeNamedParam (MaybeNamedParam &&other) = default;
  MaybeNamedParam &operator= (MaybeNamedParam &&other) = default;

  std::string as_string () const;

  // Returns whether the param is in an error state.
  bool is_error () const { return param_type == nullptr; }

  // Creates an error state param.
  static MaybeNamedParam create_error ()
  {
    return MaybeNamedParam ({""}, UNNAMED, nullptr, UNDEF_LOCATION);
  }

  location_t get_locus () const { return locus; }

  std::unique_ptr<Type> &get_type () { return param_type; }

  ParamKind get_param_kind () const { return param_kind; }

  Identifier get_name () const { return name; }
};

std::string enum_to_str (MaybeNamedParam::ParamKind);

/* A function pointer type - can be created via coercion from function items and
 * non- capturing closures. */
class BareFunctionType : public TypeNoBounds
{
  // bool has_for_lifetimes;
  // ForLifetimes for_lifetimes;
  std::vector<LifetimeParam> for_lifetimes; // inlined version

  FunctionQualifiers function_qualifiers;
  std::vector<MaybeNamedParam> params;
  bool is_variadic;

  std::unique_ptr<Type> return_type; // inlined version

public:
  // Whether a return type is defined with the function.
  bool has_return_type () const { return return_type != nullptr; }

  // Whether the function has ForLifetimes.
  bool has_for_lifetimes () const { return !for_lifetimes.empty (); }

  BareFunctionType (Analysis::NodeMapping mappings,
		    std::vector<LifetimeParam> lifetime_params,
		    FunctionQualifiers qualifiers,
		    std::vector<MaybeNamedParam> named_params, bool is_variadic,
		    std::unique_ptr<Type> type, location_t locus)
    : TypeNoBounds (mappings, locus),
      for_lifetimes (std::move (lifetime_params)),
      function_qualifiers (std::move (qualifiers)),
      params (std::move (named_params)), is_variadic (is_variadic),
      return_type (std::move (type))
  {}

  // Copy constructor with clone
  BareFunctionType (BareFunctionType const &other)
    : TypeNoBounds (other.mappings, other.locus),
      for_lifetimes (other.for_lifetimes),
      function_qualifiers (other.function_qualifiers), params (other.params),
      is_variadic (other.is_variadic),
      return_type (other.return_type->clone_type ())
  {}

  // Overload assignment operator to deep copy
  BareFunctionType &operator= (BareFunctionType const &other)
  {
    mappings = other.mappings;
    for_lifetimes = other.for_lifetimes;
    function_qualifiers = other.function_qualifiers;
    params = other.params;
    is_variadic = other.is_variadic;
    return_type = other.return_type->clone_type ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  BareFunctionType (BareFunctionType &&other) = default;
  BareFunctionType &operator= (BareFunctionType &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;

  std::vector<LifetimeParam> &get_for_lifetimes () { return for_lifetimes; }
  bool get_is_variadic () { return is_variadic; }
  FunctionQualifiers &get_function_qualifiers () { return function_qualifiers; }

  std::vector<MaybeNamedParam> &get_function_params () { return params; }
  const std::vector<MaybeNamedParam> &get_function_params () const
  {
    return params;
  }

  // TODO: would a "vis_type" be better?
  std::unique_ptr<Type> &get_return_type () { return return_type; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BareFunctionType *clone_type_impl () const override
  {
    return new BareFunctionType (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BareFunctionType *clone_type_no_bounds_impl () const override
  {
    return new BareFunctionType (*this);
  }
};

/* TODO: possible types
 * struct type?
 * "enum" (tagged union) type?
 * C-like union type?
 * function item type?
 * closure expression types?
 * primitive types (bool, int, float, char, str (the slice))
 * Although supposedly TypePaths are used to reference these types (including
 * primitives) */

/* FIXME: Incomplete spec references:
 *  anonymous type parameters, aka "impl Trait in argument position" - impl then
 * trait bounds abstract return types, aka "impl Trait in return position" -
 * impl then trait bounds */
} // namespace HIR
} // namespace Rust

#endif
