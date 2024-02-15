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

#ifndef RUST_AST_TYPE_H
#define RUST_AST_TYPE_H

#include "rust-ast.h"
#include "rust-path.h"

namespace Rust {
namespace AST {
// definitions moved to rust-ast.h
class TypeParamBound;
class Lifetime;

// A trait bound
class TraitBound : public TypeParamBound
{
  bool in_parens;
  bool opening_question_mark;

  // bool has_for_lifetimes;
  // LifetimeParams for_lifetimes;
  std::vector<LifetimeParam> for_lifetimes; // inlined LifetimeParams

  TypePath type_path;

  location_t locus;

public:
  // Returns whether trait bound has "for" lifetimes
  bool has_for_lifetimes () const { return !for_lifetimes.empty (); }

  std::vector<LifetimeParam> &get_for_lifetimes () { return for_lifetimes; }

  TraitBound (TypePath type_path, location_t locus, bool in_parens = false,
	      bool opening_question_mark = false,
	      std::vector<LifetimeParam> for_lifetimes
	      = std::vector<LifetimeParam> ())
    : TypeParamBound (Analysis::Mappings::get ()->get_next_node_id ()),
      in_parens (in_parens), opening_question_mark (opening_question_mark),
      for_lifetimes (std::move (for_lifetimes)),
      type_path (std::move (type_path)), locus (locus)
  {}

  TraitBound (NodeId id, TypePath type_path, location_t locus,
	      bool in_parens = false, bool opening_question_mark = false,
	      std::vector<LifetimeParam> for_lifetimes
	      = std::vector<LifetimeParam> ())
    : TypeParamBound (id), in_parens (in_parens),
      opening_question_mark (opening_question_mark),
      for_lifetimes (std::move (for_lifetimes)),
      type_path (std::move (type_path)), locus (locus)
  {}

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this mutable getter seems kinda dodgy
  TypePath &get_type_path () { return type_path; }
  const TypePath &get_type_path () const { return type_path; }

  bool is_in_parens () const { return in_parens; }
  bool has_opening_question_mark () const { return opening_question_mark; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TraitBound *clone_type_param_bound_impl () const override
  {
    return new TraitBound (node_id, type_path, locus, in_parens,
			   opening_question_mark, for_lifetimes);
  }
};

// definition moved to rust-ast.h
class TypeNoBounds;

// An impl trait? Poor reference material here.
class ImplTraitType : public Type
{
  // TypeParamBounds type_param_bounds;
  // inlined form
  std::vector<std::unique_ptr<TypeParamBound> > type_param_bounds;

  location_t locus;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ImplTraitType *clone_type_impl () const override
  {
    return new ImplTraitType (*this);
  }

public:
  ImplTraitType (
    std::vector<std::unique_ptr<TypeParamBound> > type_param_bounds,
    location_t locus)
    : type_param_bounds (std::move (type_param_bounds)), locus (locus)
  {}

  // copy constructor with vector clone
  ImplTraitType (ImplTraitType const &other) : locus (other.locus)
  {
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());
  }

  // overloaded assignment operator to clone
  ImplTraitType &operator= (ImplTraitType const &other)
  {
    locus = other.locus;

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    return *this;
  }

  // move constructors
  ImplTraitType (ImplTraitType &&other) = default;
  ImplTraitType &operator= (ImplTraitType &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: mutable getter seems kinda dodgy
  std::vector<std::unique_ptr<TypeParamBound> > &get_type_param_bounds ()
  {
    return type_param_bounds;
  }
  const std::vector<std::unique_ptr<TypeParamBound> > &
  get_type_param_bounds () const
  {
    return type_param_bounds;
  }
};

// An opaque value of another type that implements a set of traits
class TraitObjectType : public Type
{
  bool has_dyn;
  std::vector<std::unique_ptr<TypeParamBound> > type_param_bounds;
  location_t locus;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TraitObjectType *clone_type_impl () const override
  {
    return new TraitObjectType (*this);
  }

public:
  TraitObjectType (
    std::vector<std::unique_ptr<TypeParamBound> > type_param_bounds,
    location_t locus, bool is_dyn_dispatch)
    : has_dyn (is_dyn_dispatch),
      type_param_bounds (std::move (type_param_bounds)), locus (locus)
  {}

  // copy constructor with vector clone
  TraitObjectType (TraitObjectType const &other)
    : has_dyn (other.has_dyn), locus (other.locus)
  {
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());
  }

  // overloaded assignment operator to clone
  TraitObjectType &operator= (TraitObjectType const &other)
  {
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

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  bool is_dyn () const { return has_dyn; }

  // TODO: mutable getter seems kinda dodgy
  std::vector<std::unique_ptr<TypeParamBound> > &get_type_param_bounds ()
  {
    return type_param_bounds;
  }
  const std::vector<std::unique_ptr<TypeParamBound> > &
  get_type_param_bounds () const
  {
    return type_param_bounds;
  }
};

// A type with parentheses around it, used to avoid ambiguity.
class ParenthesisedType : public TypeNoBounds
{
  std::unique_ptr<Type> type_in_parens;
  location_t locus;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ParenthesisedType *clone_type_no_bounds_impl () const override
  {
    return new ParenthesisedType (*this);
  }

public:
  // Constructor uses Type pointer for polymorphism
  ParenthesisedType (std::unique_ptr<Type> type_inside_parens, location_t locus)
    : type_in_parens (std::move (type_inside_parens)), locus (locus)
  {}

  /* Copy constructor uses custom deep copy method for type to preserve
   * polymorphism */
  ParenthesisedType (ParenthesisedType const &other)
    : type_in_parens (other.type_in_parens->clone_type ()), locus (other.locus)
  {}

  // overload assignment operator to use custom clone method
  ParenthesisedType &operator= (ParenthesisedType const &other)
  {
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
  TraitBound *to_trait_bound (bool) const override
  {
    /* NOTE: obviously it is unknown whether the internal type is a trait bound
     * due to polymorphism, so just let the internal type handle it. As
     * parenthesised type, it must be in parentheses. */
    return type_in_parens->to_trait_bound (true);
  }

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: would a "vis_type" be better?
  std::unique_ptr<Type> &get_type_in_parens ()
  {
    rust_assert (type_in_parens != nullptr);
    return type_in_parens;
  }
};

// Impl trait with a single bound? Poor reference material here.
class ImplTraitTypeOneBound : public TypeNoBounds
{
  TraitBound trait_bound;
  location_t locus;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ImplTraitTypeOneBound *clone_type_no_bounds_impl () const override
  {
    return new ImplTraitTypeOneBound (*this);
  }

public:
  ImplTraitTypeOneBound (TraitBound trait_bound, location_t locus)
    : trait_bound (std::move (trait_bound)), locus (locus)
  {}

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: would a "vis_type" be better?
  TraitBound &get_trait_bound ()
  {
    // TODO: check to ensure invariants are met?
    return trait_bound;
  }
};

/* A trait object with a single trait bound. The "trait bound" is really just
 * the trait. Basically like using an interface as a type in an OOP language. */
class TraitObjectTypeOneBound : public TypeNoBounds
{
  bool has_dyn;
  TraitBound trait_bound;
  location_t locus;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TraitObjectTypeOneBound *clone_type_no_bounds_impl () const override
  {
    return new TraitObjectTypeOneBound (*this);
  }

public:
  TraitObjectTypeOneBound (TraitBound trait_bound, location_t locus,
			   bool is_dyn_dispatch = false)
    : has_dyn (is_dyn_dispatch), trait_bound (std::move (trait_bound)),
      locus (locus)
  {}

  std::string as_string () const override;

  // Creates a trait bound (clone of this one's trait bound) - HACK
  TraitBound *to_trait_bound (bool) const override
  {
    /* NOTE: this assumes there is no dynamic dispatch specified- if there was,
     * this cloning would not be required as parsing is unambiguous. */
    return new TraitBound (trait_bound);
  }

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: would a "vis_type" be better?
  TraitBound &get_trait_bound ()
  {
    // TODO: check to ensure invariants are met?
    return trait_bound;
  }

  bool is_dyn () const { return has_dyn; }
};

class TypePath; // definition moved to "rust-path.h"

/* A type consisting of the "product" of others (the tuple's elements) in a
 * specific order */
class TupleType : public TypeNoBounds
{
  std::vector<std::unique_ptr<Type> > elems;
  location_t locus;

public:
  // Returns whether the tuple type is the unit type, i.e. has no elements.
  bool is_unit_type () const { return elems.empty (); }

  TupleType (std::vector<std::unique_ptr<Type> > elems, location_t locus)
    : elems (std::move (elems)), locus (locus)
  {}

  // copy constructor with vector clone
  TupleType (TupleType const &other) : locus (other.locus)
  {
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

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: mutable getter seems kinda dodgy
  std::vector<std::unique_ptr<Type> > &get_elems () { return elems; }
  const std::vector<std::unique_ptr<Type> > &get_elems () const
  {
    return elems;
  }

protected:
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
  location_t locus;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  NeverType *clone_type_no_bounds_impl () const override
  {
    return new NeverType (*this);
  }

public:
  NeverType (location_t locus) : locus (locus) {}

  std::string as_string () const override { return "! (never type)"; }

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;
};

// A type consisting of a pointer without safety or liveness guarantees
class RawPointerType : public TypeNoBounds
{
public:
  enum PointerType
  {
    MUT,
    CONST
  };

private:
  PointerType pointer_type;
  std::unique_ptr<TypeNoBounds> type;
  location_t locus;

public:
  // Returns whether the pointer is mutable or constant.
  PointerType get_pointer_type () const { return pointer_type; }

  // Constructor requires pointer for polymorphism reasons
  RawPointerType (PointerType pointer_type,
		  std::unique_ptr<TypeNoBounds> type_no_bounds,
		  location_t locus)
    : pointer_type (pointer_type), type (std::move (type_no_bounds)),
      locus (locus)
  {}

  // Copy constructor calls custom polymorphic clone function
  RawPointerType (RawPointerType const &other)
    : pointer_type (other.pointer_type),
      type (other.type->clone_type_no_bounds ()), locus (other.locus)
  {}

  // overload assignment operator to use custom clone method
  RawPointerType &operator= (RawPointerType const &other)
  {
    pointer_type = other.pointer_type;
    type = other.type->clone_type_no_bounds ();
    locus = other.locus;
    return *this;
  }

  // default move semantics
  RawPointerType (RawPointerType &&other) = default;
  RawPointerType &operator= (RawPointerType &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: would a "vis_type" be better?
  std::unique_ptr<TypeNoBounds> &get_type_pointed_to ()
  {
    rust_assert (type != nullptr);
    return type;
  }

protected:
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

  bool has_mut;
  std::unique_ptr<TypeNoBounds> type;
  location_t locus;

public:
  // Returns whether the reference is mutable or immutable.
  bool is_mut () const { return has_mut; }

  // Returns whether the reference has a lifetime.
  bool has_lifetime () const { return !lifetime.is_error (); }

  // Constructor
  ReferenceType (bool is_mut, std::unique_ptr<TypeNoBounds> type_no_bounds,
		 location_t locus, Lifetime lifetime = Lifetime::elided ())
    : lifetime (std::move (lifetime)), has_mut (is_mut),
      type (std::move (type_no_bounds)), locus (locus)
  {}

  // Copy constructor with custom clone method
  ReferenceType (ReferenceType const &other)
    : lifetime (other.lifetime), has_mut (other.has_mut),
      type (other.type->clone_type_no_bounds ()), locus (other.locus)
  {}

  // Operator overload assignment operator to custom clone the unique pointer
  ReferenceType &operator= (ReferenceType const &other)
  {
    lifetime = other.lifetime;
    has_mut = other.has_mut;
    type = other.type->clone_type_no_bounds ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  ReferenceType (ReferenceType &&other) = default;
  ReferenceType &operator= (ReferenceType &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: would a "vis_type" be better?
  std::unique_ptr<TypeNoBounds> &get_type_referenced ()
  {
    rust_assert (type != nullptr);
    return type;
  }

  bool get_has_mut () const { return has_mut; }

  Lifetime &get_lifetime () { return lifetime; }

  std::unique_ptr<TypeNoBounds> &get_base_type () { return type; }

protected:
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
  location_t locus;

public:
  // Constructor requires pointers for polymorphism
  ArrayType (std::unique_ptr<Type> type, std::unique_ptr<Expr> array_size,
	     location_t locus)
    : elem_type (std::move (type)), size (std::move (array_size)), locus (locus)
  {}

  // Copy constructor requires deep copies of both unique pointers
  ArrayType (ArrayType const &other)
    : elem_type (other.elem_type->clone_type ()),
      size (other.size->clone_expr ()), locus (other.locus)
  {}

  // Overload assignment operator to deep copy pointers
  ArrayType &operator= (ArrayType const &other)
  {
    elem_type = other.elem_type->clone_type ();
    size = other.size->clone_expr ();
    locus = other.locus;
    return *this;
  }

  // move constructors
  ArrayType (ArrayType &&other) = default;
  ArrayType &operator= (ArrayType &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: would a "vis_type" be better?
  std::unique_ptr<Type> &get_elem_type ()
  {
    rust_assert (elem_type != nullptr);
    return elem_type;
  }

  // TODO: would a "vis_expr" be better?
  std::unique_ptr<Expr> &get_size_expr ()
  {
    rust_assert (size != nullptr);
    return size;
  }

protected:
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
  location_t locus;

public:
  // Constructor requires pointer for polymorphism
  SliceType (std::unique_ptr<Type> type, location_t locus)
    : elem_type (std::move (type)), locus (locus)
  {}

  // Copy constructor requires deep copy of Type smart pointer
  SliceType (SliceType const &other)
    : elem_type (other.elem_type->clone_type ()), locus (other.locus)
  {}

  // Overload assignment operator to deep copy
  SliceType &operator= (SliceType const &other)
  {
    elem_type = other.elem_type->clone_type ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  SliceType (SliceType &&other) = default;
  SliceType &operator= (SliceType &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: would a "vis_type" be better?
  std::unique_ptr<Type> &get_elem_type ()
  {
    rust_assert (elem_type != nullptr);
    return elem_type;
  }

protected:
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
  location_t locus;

  // e.g. Vec<_> = whatever
protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  InferredType *clone_type_no_bounds_impl () const override
  {
    return new InferredType (*this);
  }

public:
  InferredType (location_t locus) : locus (locus) {}

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;
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
  std::vector<Attribute> outer_attrs;

  std::unique_ptr<Type> param_type;

  ParamKind param_kind;
  Identifier name; // technically, can be an identifier or '_'

  location_t locus;

public:
  MaybeNamedParam (Identifier name, ParamKind param_kind,
		   std::unique_ptr<Type> param_type,
		   std::vector<Attribute> outer_attrs, location_t locus)
    : outer_attrs (std::move (outer_attrs)),
      param_type (std::move (param_type)), param_kind (param_kind),
      name (std::move (name)), locus (locus)
  {}

  // Copy constructor with clone
  MaybeNamedParam (MaybeNamedParam const &other)
    : outer_attrs (other.outer_attrs), param_kind (other.param_kind),
      name (other.name), locus (other.locus)
  {
    // guard to prevent null dereference
    if (other.param_type != nullptr)
      param_type = other.param_type->clone_type ();
  }

  ~MaybeNamedParam () = default;

  // Overloaded assignment operator with clone
  MaybeNamedParam &operator= (MaybeNamedParam const &other)
  {
    outer_attrs = other.outer_attrs;
    name = other.name;
    param_kind = other.param_kind;
    locus = other.locus;

    // guard to prevent null dereference
    if (other.param_type != nullptr)
      param_type = other.param_type->clone_type ();
    else
      param_type = nullptr;

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
    return MaybeNamedParam ({""}, UNNAMED, nullptr, {}, UNDEF_LOCATION);
  }

  location_t get_locus () const { return locus; }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: would a "vis_type" be better?
  std::unique_ptr<Type> &get_type ()
  {
    rust_assert (param_type != nullptr);
    return param_type;
  }

  ParamKind get_param_kind () const { return param_kind; }

  Identifier get_name () const { return name; }
};

/* A function pointer type - can be created via coercion from function items and
 * non-capturing closures. */
class BareFunctionType : public TypeNoBounds
{
  // bool has_for_lifetimes;
  // ForLifetimes for_lifetimes;
  std::vector<LifetimeParam> for_lifetimes; // inlined version

  FunctionQualifiers function_qualifiers;
  std::vector<MaybeNamedParam> params;
  bool _is_variadic;
  std::vector<Attribute> variadic_attrs;

  // bool has_return_type;
  // BareFunctionReturnType return_type;
  std::unique_ptr<TypeNoBounds> return_type; // inlined version

  location_t locus;

public:
  // Whether a return type is defined with the function.
  bool has_return_type () const { return return_type != nullptr; }

  // Whether the function has ForLifetimes.
  bool has_for_lifetimes () const { return !for_lifetimes.empty (); }

  std::vector<LifetimeParam> &get_for_lifetimes () { return for_lifetimes; }

  bool is_variadic () const { return _is_variadic; }

  std::vector<Attribute> &get_variadic_attr () { return variadic_attrs; };
  const std::vector<Attribute> &get_variadic_attr () const
  {
    return variadic_attrs;
  };

  BareFunctionType (std::vector<LifetimeParam> lifetime_params,
		    FunctionQualifiers qualifiers,
		    std::vector<MaybeNamedParam> named_params, bool is_variadic,
		    std::vector<Attribute> variadic_attrs,
		    std::unique_ptr<TypeNoBounds> type, location_t locus)
    : for_lifetimes (std::move (lifetime_params)),
      function_qualifiers (std::move (qualifiers)),
      params (std::move (named_params)), _is_variadic (is_variadic),
      variadic_attrs (std::move (variadic_attrs)),
      return_type (std::move (type)), locus (locus)
  {
    if (!variadic_attrs.empty ())
      is_variadic = true;
  }

  // Copy constructor with clone
  BareFunctionType (BareFunctionType const &other)
    : for_lifetimes (other.for_lifetimes),
      function_qualifiers (other.function_qualifiers), params (other.params),
      _is_variadic (other._is_variadic), variadic_attrs (other.variadic_attrs),
      locus (other.locus)
  {
    // guard to prevent null dereference
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type_no_bounds ();
  }

  // Overload assignment operator to deep copy
  BareFunctionType &operator= (BareFunctionType const &other)
  {
    for_lifetimes = other.for_lifetimes;
    function_qualifiers = other.function_qualifiers;
    params = other.params;
    _is_variadic = other._is_variadic;
    variadic_attrs = other.variadic_attrs;
    locus = other.locus;

    // guard to prevent null dereference
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type_no_bounds ();
    else
      return_type = nullptr;

    return *this;
  }

  // move constructors
  BareFunctionType (BareFunctionType &&other) = default;
  BareFunctionType &operator= (BareFunctionType &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this mutable getter seems kinda dodgy
  std::vector<MaybeNamedParam> &get_function_params () { return params; }
  const std::vector<MaybeNamedParam> &get_function_params () const
  {
    return params;
  }

  // TODO: would a "vis_type" be better?
  std::unique_ptr<TypeNoBounds> &get_return_type ()
  {
    rust_assert (has_return_type ());
    return return_type;
  }

  FunctionQualifiers &get_function_qualifiers () { return function_qualifiers; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BareFunctionType *clone_type_no_bounds_impl () const override
  {
    return new BareFunctionType (*this);
  }
};

// Forward decl - defined in rust-macro.h
class MacroInvocation;

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
} // namespace AST
} // namespace Rust

#endif
