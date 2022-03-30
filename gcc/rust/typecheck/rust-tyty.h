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

#ifndef RUST_TYTY
#define RUST_TYTY

#include "rust-hir-map.h"
#include "rust-hir-full.h"
#include "rust-diagnostics.h"
#include "rust-abi.h"
#include "rust-common.h"
#include "rust-identifier.h"

namespace Rust {

namespace Resolver {
class TraitReference;
class TraitItemReference;
class AssociatedImplTrait;
} // namespace Resolver

namespace TyTy {

// https://rustc-dev-guide.rust-lang.org/type-inference.html#inference-variables
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/enum.TyKind.html#variants
enum TypeKind
{
  INFER,
  ADT,
  STR,
  REF,
  POINTER,
  PARAM,
  ARRAY,
  SLICE,
  FNDEF,
  FNPTR,
  TUPLE,
  BOOL,
  CHAR,
  INT,
  UINT,
  FLOAT,
  USIZE,
  ISIZE,
  NEVER,
  PLACEHOLDER,
  PROJECTION,
  DYNAMIC,
  CLOSURE,
  // there are more to add...
  ERROR
};

class TypeKindFormat
{
public:
  static std::string to_string (TypeKind kind);
};

class BaseType;
class TypeBoundPredicate;
class TypeBoundPredicateItem
{
public:
  TypeBoundPredicateItem (const TypeBoundPredicate *parent,
			  const Resolver::TraitItemReference *trait_item_ref)
    : parent (parent), trait_item_ref (trait_item_ref)
  {}

  static TypeBoundPredicateItem error ()
  {
    return TypeBoundPredicateItem (nullptr, nullptr);
  }

  bool is_error () const
  {
    return parent == nullptr || trait_item_ref == nullptr;
  }

  BaseType *get_tyty_for_receiver (const TyTy::BaseType *receiver,
				   const HIR::GenericArgs *bound_args
				   = nullptr);

  const Resolver::TraitItemReference *get_raw_item () const;

  bool needs_implementation () const;

  const TypeBoundPredicate *get_parent () const { return parent; }

private:
  const TypeBoundPredicate *parent;
  const Resolver::TraitItemReference *trait_item_ref;
};

class TypeBoundsMappings
{
protected:
  TypeBoundsMappings (std::vector<TypeBoundPredicate> specified_bounds);

public:
  std::vector<TypeBoundPredicate> &get_specified_bounds ();

  const std::vector<TypeBoundPredicate> &get_specified_bounds () const;

  size_t num_specified_bounds () const;

  std::string raw_bounds_as_string () const;

  std::string bounds_as_string () const;

protected:
  void add_bound (TypeBoundPredicate predicate);

  std::vector<TypeBoundPredicate> specified_bounds;
};

class TyVisitor;
class TyConstVisitor;
class BaseType : public TypeBoundsMappings
{
public:
  virtual ~BaseType () {}

  HirId get_ref () const { return ref; }

  void set_ref (HirId id)
  {
    if (id != ref)
      append_reference (ref);
    ref = id;
  }

  HirId get_ty_ref () const { return ty_ref; }

  void set_ty_ref (HirId id) { ty_ref = id; }

  virtual void accept_vis (TyVisitor &vis) = 0;

  virtual void accept_vis (TyConstVisitor &vis) const = 0;

  virtual std::string as_string () const = 0;

  virtual std::string get_name () const = 0;

  // Unify two types. Returns a pointer to the newly-created unified ty, or
  // nullptr if the two ty cannot be unified. The caller is responsible for
  // releasing the memory of the returned ty.
  virtual BaseType *unify (BaseType *other) = 0;

  // similar to unify but does not actually perform type unification but
  // determines whether they are compatible. Consider the following
  //
  // fn foo<T>() -> T { ... }
  // fn foo() -> i32 { ... }
  //
  // when the function has been substituted they can be considered equal.
  //
  // It can also be used to optional emit errors for trait item compatibility
  // checks
  virtual bool can_eq (const BaseType *other, bool emit_errors) const = 0;

  // this is the base coercion interface for types
  virtual BaseType *coerce (BaseType *other) = 0;

  // this is the cast interface for TypeCastExpr
  virtual BaseType *cast (BaseType *other) = 0;

  // Check value equality between two ty. Type inference rules are ignored. Two
  //   ty are considered equal if they're of the same kind, and
  //     1. (For ADTs, arrays, tuples, refs) have the same underlying ty
  //     2. (For functions) have the same signature
  virtual bool is_equal (const BaseType &other) const
  {
    return get_kind () == other.get_kind ();
  }

  bool satisfies_bound (const TypeBoundPredicate &predicate) const;

  bool bounds_compatible (const BaseType &other, Location locus,
			  bool emit_error) const;

  void inherit_bounds (const BaseType &other);

  void inherit_bounds (
    const std::vector<TyTy::TypeBoundPredicate> &specified_bounds);

  virtual bool is_unit () const { return false; }

  virtual bool is_concrete () const = 0;

  TypeKind get_kind () const { return kind; }

  /* Returns a pointer to a clone of this. The caller is responsible for
   * releasing the memory of the returned ty. */
  virtual BaseType *clone () const = 0;

  // get_combined_refs returns the chain of node refs involved in unification
  std::set<HirId> get_combined_refs () const { return combined; }

  void append_reference (HirId id) { combined.insert (id); }

  virtual bool supports_substitutions () const { return false; }

  virtual bool has_subsititions_defined () const { return false; }

  virtual bool can_substitute () const
  {
    return supports_substitutions () && has_subsititions_defined ();
  }

  virtual bool needs_generic_substitutions () const { return false; }

  bool contains_type_parameters () const { return !is_concrete (); }

  std::string mappings_str () const
  {
    std::string buffer = "Ref: " + std::to_string (get_ref ())
			 + " TyRef: " + std::to_string (get_ty_ref ());
    buffer += "[";
    for (auto &ref : combined)
      buffer += std::to_string (ref) + ",";
    buffer += "]";
    return "(" + buffer + ")";
  }

  std::string debug_str () const
  {
    return TypeKindFormat::to_string (get_kind ()) + ":" + as_string () + ":"
	   + mappings_str () + ":" + bounds_as_string ();
  }

  void debug () const
  {
    rust_debug ("[%p] %s", static_cast<const void *> (this),
		debug_str ().c_str ());
  }

  const BaseType *get_root () const;

  const RustIdent &get_ident () const { return ident; }

  Location get_locus () const { return ident.locus; }

protected:
  BaseType (HirId ref, HirId ty_ref, TypeKind kind, RustIdent ident,
	    std::set<HirId> refs = std::set<HirId> ())
    : TypeBoundsMappings ({}), kind (kind), ref (ref), ty_ref (ty_ref),
      combined (refs), ident (ident), mappings (Analysis::Mappings::get ())
  {}

  BaseType (HirId ref, HirId ty_ref, TypeKind kind, RustIdent ident,
	    std::vector<TypeBoundPredicate> specified_bounds,
	    std::set<HirId> refs = std::set<HirId> ())
    : TypeBoundsMappings (specified_bounds), kind (kind), ref (ref),
      ty_ref (ty_ref), combined (refs), ident (ident),
      mappings (Analysis::Mappings::get ())
  {}

  TypeKind kind;
  HirId ref;
  HirId ty_ref;
  std::set<HirId> combined;
  RustIdent ident;

  Analysis::Mappings *mappings;
};

// this is a placeholder for types that can change like inference variables
class TyVar
{
public:
  explicit TyVar (HirId ref);

  HirId get_ref () const { return ref; }

  BaseType *get_tyty () const;

  static TyVar get_implicit_infer_var (Location locus);

private:
  HirId ref;
};

class InferType : public BaseType
{
public:
  enum InferTypeKind
  {
    GENERAL,
    INTEGRAL,
    FLOAT
  };

  InferType (HirId ref, InferTypeKind infer_kind, Location locus,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::INFER,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      infer_kind (infer_kind)
  {}

  InferType (HirId ref, HirId ty_ref, InferTypeKind infer_kind, Location locus,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::INFER,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      infer_kind (infer_kind)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  BaseType *clone () const final override;

  InferTypeKind get_infer_kind () const { return infer_kind; }

  std::string get_name () const override final { return as_string (); }

  bool default_type (BaseType **type) const;

  bool is_concrete () const final override { return true; }

private:
  InferTypeKind infer_kind;
};

class ErrorType : public BaseType
{
public:
  ErrorType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ERROR,
		{Resolver::CanonicalPath::create_empty (), Location ()}, refs)
  {}

  ErrorType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ERROR,
		{Resolver::CanonicalPath::create_empty (), Location ()}, refs)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  bool is_unit () const override { return true; }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  BaseType *clone () const final override;

  std::string get_name () const override final { return as_string (); }

  bool is_concrete () const final override { return false; }
};

class SubstitutionArgumentMappings;
class ParamType : public BaseType
{
public:
  ParamType (std::string symbol, Location locus, HirId ref,
	     HIR::GenericParam &param,
	     std::vector<TypeBoundPredicate> specified_bounds,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::PARAM,
		{Resolver::CanonicalPath::new_seg (UNKNOWN_NODEID, symbol),
		 locus},
		specified_bounds, refs),
      symbol (symbol), param (param)
  {}

  ParamType (std::string symbol, Location locus, HirId ref, HirId ty_ref,
	     HIR::GenericParam &param,
	     std::vector<TypeBoundPredicate> specified_bounds,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::PARAM,
		{Resolver::CanonicalPath::new_seg (UNKNOWN_NODEID, symbol),
		 locus},
		specified_bounds, refs),
      symbol (symbol), param (param)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  BaseType *clone () const final override;

  std::string get_symbol () const;

  HIR::GenericParam &get_generic_param () { return param; }

  bool can_resolve () const { return get_ref () != get_ty_ref (); }

  BaseType *resolve () const;

  std::string get_name () const override final;

  bool is_equal (const BaseType &other) const override;

  bool is_concrete () const override final
  {
    if (!can_resolve ())
      return false;

    auto r = resolve ();
    return r->is_concrete ();
  }

  ParamType *handle_substitions (SubstitutionArgumentMappings mappings);

private:
  std::string symbol;
  HIR::GenericParam &param;
};

class StructFieldType
{
public:
  StructFieldType (HirId ref, std::string name, BaseType *ty)
    : ref (ref), name (name), ty (ty)
  {}

  HirId get_ref () const { return ref; }

  std::string as_string () const;

  bool is_equal (const StructFieldType &other) const;

  std::string get_name () const { return name; }

  BaseType *get_field_type () const { return ty; }

  void set_field_type (BaseType *fty) { ty = fty; }

  StructFieldType *clone () const;

  bool is_concrete () const { return ty->is_concrete (); }

  void debug () const { rust_debug ("%s", as_string ().c_str ()); }

private:
  HirId ref;
  std::string name;
  BaseType *ty;
};

class TupleType : public BaseType
{
public:
  TupleType (HirId ref, Location locus,
	     std::vector<TyVar> fields = std::vector<TyVar> (),
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::TUPLE,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      fields (fields)
  {}

  TupleType (HirId ref, HirId ty_ref, Location locus,
	     std::vector<TyVar> fields = std::vector<TyVar> (),
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::TUPLE,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      fields (fields)
  {}

  static TupleType *get_unit_type (HirId ref)
  {
    return new TupleType (ref, Linemap::predeclared_location ());
  }

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  bool is_unit () const override { return this->fields.empty (); }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  size_t num_fields () const { return fields.size (); }

  BaseType *get_field (size_t index) const;

  BaseType *clone () const final override;

  bool is_concrete () const override final
  {
    for (size_t i = 0; i < num_fields (); i++)
      {
	if (!get_field (i)->is_concrete ())
	  return false;
      }
    return true;
  }

  const std::vector<TyVar> &get_fields () const { return fields; }

  std::string get_name () const override final { return as_string (); }

  TupleType *handle_substitions (SubstitutionArgumentMappings mappings);

private:
  std::vector<TyVar> fields;
};

class SubstitutionParamMapping
{
public:
  SubstitutionParamMapping (const HIR::TypeParam &generic, ParamType *param)
    : generic (generic), param (param)
  {}

  SubstitutionParamMapping (const SubstitutionParamMapping &other)
    : generic (other.generic), param (other.param)
  {}

  std::string as_string () const
  {
    if (param == nullptr)
      return "nullptr";

    return param->get_name ();
  }

  bool fill_param_ty (BaseType &type, Location locus);

  SubstitutionParamMapping clone () const
  {
    return SubstitutionParamMapping (generic, static_cast<ParamType *> (
						param->clone ()));
  }

  const ParamType *get_param_ty () const { return param; }

  const HIR::TypeParam &get_generic_param () { return generic; };

  // this is used for the backend to override the HirId ref of the param to
  // what the concrete type is for the rest of the context
  void override_context ();

  bool needs_substitution () const
  {
    auto p = get_param_ty ();
    if (!p->can_resolve ())
      return true;

    return p->resolve ()->get_kind () == TypeKind::PARAM;
  }

  Location get_param_locus () const { return generic.get_locus (); }

  bool param_has_default_ty () const { return generic.has_type (); }

  BaseType *get_default_ty () const
  {
    TyVar var (generic.get_type_mappings ().get_hirid ());
    return var.get_tyty ();
  }

  bool need_substitution () const;

private:
  const HIR::TypeParam &generic;
  ParamType *param;
};

class SubstitutionArg
{
public:
  SubstitutionArg (const SubstitutionParamMapping *param, BaseType *argument)
    : param (std::move (param)), argument (argument)
  {}

  SubstitutionArg (const SubstitutionArg &other)
    : param (other.param), argument (other.argument)
  {}

  SubstitutionArg &operator= (const SubstitutionArg &other)
  {
    param = other.param;
    argument = other.argument;
    return *this;
  }

  BaseType *get_tyty () { return argument; }

  const BaseType *get_tyty () const { return argument; }

  const SubstitutionParamMapping *get_param_mapping () const { return param; }

  static SubstitutionArg error () { return SubstitutionArg (nullptr, nullptr); }

  bool is_error () const { return param == nullptr || argument == nullptr; }

  bool is_conrete () const
  {
    if (argument != nullptr)
      return true;

    if (argument->get_kind () == TyTy::TypeKind::PARAM)
      return false;

    return argument->is_concrete ();
  }

  std::string as_string () const
  {
    return param->as_string ()
	   + (argument != nullptr ? ":" + argument->as_string () : "");
  }

private:
  const SubstitutionParamMapping *param;
  BaseType *argument;
};

class SubstitutionArgumentMappings
{
public:
  SubstitutionArgumentMappings (std::vector<SubstitutionArg> mappings,
				Location locus)
    : mappings (mappings), locus (locus)
  {}

  SubstitutionArgumentMappings (const SubstitutionArgumentMappings &other)
    : mappings (other.mappings), locus (other.locus)
  {}

  SubstitutionArgumentMappings &
  operator= (const SubstitutionArgumentMappings &other)
  {
    mappings = other.mappings;
    locus = other.locus;
    return *this;
  }

  static SubstitutionArgumentMappings error ()
  {
    return SubstitutionArgumentMappings ({}, Location ());
  }

  bool is_error () const { return mappings.size () == 0; }

  bool get_argument_for_symbol (const ParamType *param_to_find,
				SubstitutionArg *argument)
  {
    for (auto &mapping : mappings)
      {
	const SubstitutionParamMapping *param = mapping.get_param_mapping ();
	const ParamType *p = param->get_param_ty ();

	if (p->get_symbol ().compare (param_to_find->get_symbol ()) == 0)
	  {
	    *argument = mapping;
	    return true;
	  }
      }
    return false;
  }

  bool get_argument_at (size_t index, SubstitutionArg *argument)
  {
    if (index > mappings.size ())
      return false;

    *argument = mappings.at (index);
    return true;
  }

  // is_concrete means if the used args is non error, ie: non empty this will
  // verify if actual real types have been put in place of are they still
  // ParamTy
  bool is_concrete () const
  {
    for (auto &mapping : mappings)
      {
	if (!mapping.is_conrete ())
	  return false;
      }
    return true;
  }

  Location get_locus () const { return locus; }

  size_t size () const { return mappings.size (); }

  bool is_empty () const { return size () == 0; }

  std::vector<SubstitutionArg> &get_mappings () { return mappings; }

  const std::vector<SubstitutionArg> &get_mappings () const { return mappings; }

  std::string as_string () const
  {
    std::string buffer;
    for (auto &mapping : mappings)
      {
	buffer += mapping.as_string () + ", ";
      }
    return "<" + buffer + ">";
  }

private:
  std::vector<SubstitutionArg> mappings;
  Location locus;
};

class SubstitutionRef
{
public:
  SubstitutionRef (std::vector<SubstitutionParamMapping> substitutions,
		   SubstitutionArgumentMappings arguments)
    : substitutions (substitutions), used_arguments (arguments)
  {}

  bool has_substitutions () const { return substitutions.size () > 0; }

  std::string subst_as_string () const
  {
    std::string buffer;
    for (size_t i = 0; i < substitutions.size (); i++)
      {
	const SubstitutionParamMapping &sub = substitutions.at (i);
	buffer += sub.as_string ();

	if ((i + 1) < substitutions.size ())
	  buffer += ", ";
      }

    return buffer.empty () ? "" : "<" + buffer + ">";
  }

  size_t get_num_substitutions () const { return substitutions.size (); }

  std::vector<SubstitutionParamMapping> &get_substs () { return substitutions; }

  const std::vector<SubstitutionParamMapping> &get_substs () const
  {
    return substitutions;
  }

  std::vector<SubstitutionParamMapping> clone_substs () const
  {
    std::vector<SubstitutionParamMapping> clone;

    for (auto &sub : substitutions)
      clone.push_back (sub.clone ());

    return clone;
  }

  void override_context ()
  {
    for (auto &sub : substitutions)
      {
	sub.override_context ();
      }
  }

  bool needs_substitution () const
  {
    for (auto &sub : substitutions)
      {
	if (sub.need_substitution ())
	  return true;
      }
    return false;
  }

  bool was_substituted () const { return !needs_substitution (); }

  SubstitutionArgumentMappings get_substitution_arguments () const
  {
    return used_arguments;
  }

  // this is the count of type params that are not substituted fuly
  size_t num_required_substitutions () const
  {
    size_t n = 0;
    for (auto &p : substitutions)
      {
	if (p.needs_substitution ())
	  n++;
      }
    return n;
  }

  // this is the count of type params that need substituted taking into account
  // possible defaults
  size_t min_required_substitutions () const
  {
    size_t n = 0;
    for (auto &p : substitutions)
      {
	if (p.needs_substitution () && !p.param_has_default_ty ())
	  n++;
      }
    return n;
  }

  // We are trying to subst <i32, f32> into Struct Foo<X,Y> {}
  // in the case of Foo<i32,f32>{...}
  //
  // the substitions we have here define X,Y but the arguments have no bindings
  // so its a matter of ordering
  SubstitutionArgumentMappings
  get_mappings_from_generic_args (HIR::GenericArgs &args);

  // Recursive substitutions
  // Foo <A,B> { a:A, b: B}; Bar <X,Y,Z>{a:X, b: Foo<Y,Z>}
  //
  // we have bindings for X Y Z and need to propagate the binding Y,Z into Foo
  // Which binds to A,B
  SubstitutionArgumentMappings
  adjust_mappings_for_this (SubstitutionArgumentMappings &mappings);

  // Are the mappings here actually bound to this type. For example imagine the
  // case:
  //
  // struct Foo<T>(T);
  // impl<T> Foo<T> {
  //   fn test(self) { ... }
  // }
  //
  // In this case we have a generic ADT of Foo and an impl block of a generic T
  // on Foo for the Self type. When we it comes to path resolution we can have:
  //
  // Foo::<i32>::test()
  //
  // This means the first segment of Foo::<i32> returns the ADT Foo<i32> not the
  // Self ADT bound to the T from the impl block. This means when it comes to
  // the next segment of test which resolves to the function we need to check
  // wether the arguments in the struct definition of foo can be bound here
  // before substituting the previous segments type here. This functions acts as
  // a guard for the solve_mappings_from_receiver_for_self to handle the case
  // where arguments are not bound. This is important for this next case:
  //
  // struct Baz<A, B>(A, B);
  // impl Baz<i32, f32> {
  //   fn test<X>(a: X) -> X {
  //       a
  //   }
  // }
  //
  // In this case Baz has been already substituted for the impl's Self to become
  // ADT<i32, f32> so that the function test only has 1 generic argument of X.
  // The path for this will be:
  //
  // Baz::test::<_>(123)
  //
  // So the first segment here will be Baz<_, _> to try and infer the arguments
  // which will be taken from the impl's Self type in this case since it is
  // already substituted and like the previous case the check to see if we need
  // to inherit the previous segments generic arguments takes place but the
  // generic arguments are not bound to this type as they have already been
  // substituted.
  //
  // Its important to remember from the first example the FnType actually looks
  // like:
  //
  // fn <T>test(self :Foo<T>(T))
  //
  // As the generic parameters are "bound" to each of the items in the impl
  // block. So this check is about wether the arguments we have here can
  // actually be bound to this type.
  bool are_mappings_bound (SubstitutionArgumentMappings &mappings);

  // struct Foo<A, B>(A, B);
  //
  // impl<T> Foo<T, f32>;
  //     -> fn test<X>(self, a: X) -> X
  //
  // We might invoke this via:
  //
  // a = Foo(123, 456f32);
  // b = a.test::<bool>(false);
  //
  // we need to figure out relevant generic arguemts for self to apply to the
  // fntype
  SubstitutionArgumentMappings solve_mappings_from_receiver_for_self (
    SubstitutionArgumentMappings &mappings) const;

  // TODO comment
  SubstitutionArgumentMappings
  solve_missing_mappings_from_this (SubstitutionRef &ref, SubstitutionRef &to);

  // TODO comment
  BaseType *infer_substitions (Location locus)
  {
    std::vector<SubstitutionArg> args;
    for (auto &p : get_substs ())
      {
	if (p.needs_substitution ())
	  {
	    TyVar infer_var = TyVar::get_implicit_infer_var (locus);
	    args.push_back (SubstitutionArg (&p, infer_var.get_tyty ()));
	  }
	else
	  {
	    args.push_back (
	      SubstitutionArg (&p, p.get_param_ty ()->resolve ()));
	  }
      }

    SubstitutionArgumentMappings infer_arguments (std::move (args), locus);
    return handle_substitions (std::move (infer_arguments));
  }

  virtual BaseType *handle_substitions (SubstitutionArgumentMappings mappings)
    = 0;

  SubstitutionArgumentMappings get_used_arguments () const
  {
    return used_arguments;
  }

protected:
  std::vector<SubstitutionParamMapping> substitutions;
  SubstitutionArgumentMappings used_arguments;
};

class TypeBoundPredicate : public SubstitutionRef
{
public:
  TypeBoundPredicate (const Resolver::TraitReference &trait_reference,
		      Location locus);

  TypeBoundPredicate (DefId reference,
		      std::vector<SubstitutionParamMapping> substitutions,
		      Location locus);

  TypeBoundPredicate (const TypeBoundPredicate &other);

  TypeBoundPredicate &operator= (const TypeBoundPredicate &other);

  static TypeBoundPredicate error ();

  std::string as_string () const;

  const Resolver::TraitReference *get () const;

  Location get_locus () const { return locus; }

  std::string get_name () const;

  // check that this predicate is object-safe see:
  // https://doc.rust-lang.org/reference/items/traits.html#object-safety
  bool is_object_safe (bool emit_error, Location locus) const;

  void apply_generic_arguments (HIR::GenericArgs *generic_args);

  bool contains_item (const std::string &search) const;

  TypeBoundPredicateItem
  lookup_associated_item (const std::string &search) const;

  HIR::GenericArgs *get_generic_args () { return &args; }

  const HIR::GenericArgs *get_generic_args () const { return &args; }

  bool has_generic_args () const { return args.has_generic_args (); }

  // WARNING THIS WILL ALWAYS RETURN NULLPTR
  BaseType *
  handle_substitions (SubstitutionArgumentMappings mappings) override final;

  bool is_error () const;

  bool requires_generic_args () const;

private:
  DefId reference;
  Location locus;
  HIR::GenericArgs args;
  bool error_flag;
};

// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/struct.VariantDef.html
class VariantDef
{
public:
  enum VariantType
  {
    NUM,
    TUPLE,
    STRUCT
  };

  static std::string variant_type_string (VariantType type)
  {
    switch (type)
      {
      case NUM:
	return "enumeral";
      case TUPLE:
	return "tuple";
      case STRUCT:
	return "struct";
      }
    gcc_unreachable ();
    return "";
  }

  VariantDef (HirId id, std::string identifier, RustIdent ident,
	      HIR::Expr *discriminant)
    : id (id), identifier (identifier), ident (ident),
      discriminant (discriminant)

  {
    type = VariantType::NUM;
    fields = {};
  }

  VariantDef (HirId id, std::string identifier, RustIdent ident,
	      VariantType type, HIR::Expr *discriminant,
	      std::vector<StructFieldType *> fields)
    : id (id), identifier (identifier), ident (ident), type (type),
      discriminant (discriminant), fields (fields)
  {
    rust_assert (
      (type == VariantType::NUM && fields.empty ())
      || (type == VariantType::TUPLE || type == VariantType::STRUCT));
  }

  VariantDef (const VariantDef &other)
    : id (other.id), identifier (other.identifier), ident (other.ident),
      type (other.type), discriminant (other.discriminant),
      fields (other.fields)
  {}

  VariantDef &operator= (const VariantDef &other)
  {
    id = other.id;
    identifier = other.identifier;
    type = other.type;
    discriminant = other.discriminant;
    fields = other.fields;
    ident = other.ident;

    return *this;
  }

  static VariantDef &get_error_node ()
  {
    static VariantDef node
      = VariantDef (UNKNOWN_HIRID, "",
		    {Resolver::CanonicalPath::create_empty (),
		     Linemap::unknown_location ()},
		    nullptr);

    return node;
  }

  bool is_error () const { return get_id () == UNKNOWN_HIRID; }

  HirId get_id () const { return id; }

  VariantType get_variant_type () const { return type; }
  bool is_data_variant () const { return type != VariantType::NUM; }
  bool is_dataless_variant () const { return type == VariantType::NUM; }

  std::string get_identifier () const { return identifier; }

  size_t num_fields () const { return fields.size (); }
  StructFieldType *get_field_at_index (size_t index)
  {
    rust_assert (index < fields.size ());
    return fields.at (index);
  }

  std::vector<StructFieldType *> &get_fields ()
  {
    rust_assert (type != NUM);
    return fields;
  }

  bool lookup_field (const std::string &lookup, StructFieldType **field_lookup,
		     size_t *index) const
  {
    size_t i = 0;
    for (auto &field : fields)
      {
	if (field->get_name ().compare (lookup) == 0)
	  {
	    if (index != nullptr)
	      *index = i;

	    if (field_lookup != nullptr)
	      *field_lookup = field;

	    return true;
	  }
	i++;
      }
    return false;
  }

  HIR::Expr *get_discriminant () const
  {
    rust_assert (discriminant != nullptr);
    return discriminant;
  }

  std::string as_string () const
  {
    if (type == VariantType::NUM)
      return identifier + " = " + discriminant->as_string ();

    std::string buffer;
    for (size_t i = 0; i < fields.size (); ++i)
      {
	buffer += fields.at (i)->as_string ();
	if ((i + 1) < fields.size ())
	  buffer += ", ";
      }

    if (type == VariantType::TUPLE)
      return identifier + " (" + buffer + ")";
    else
      return identifier + " {" + buffer + "}";
  }

  bool is_equal (const VariantDef &other) const
  {
    if (type != other.type)
      return false;

    if (identifier.compare (other.identifier) != 0)
      return false;

    if (discriminant != other.discriminant)
      return false;

    if (fields.size () != other.fields.size ())
      return false;

    for (size_t i = 0; i < fields.size (); i++)
      {
	if (!fields.at (i)->is_equal (*other.fields.at (i)))
	  return false;
      }

    return true;
  }

  VariantDef *clone () const
  {
    std::vector<StructFieldType *> cloned_fields;
    for (auto &f : fields)
      cloned_fields.push_back ((StructFieldType *) f->clone ());

    return new VariantDef (id, identifier, ident, type, discriminant,
			   cloned_fields);
  }

  const RustIdent &get_ident () const { return ident; }

private:
  HirId id;
  std::string identifier;
  RustIdent ident;
  VariantType type;
  // can either be a structure or a discriminant value
  HIR::Expr *discriminant;
  std::vector<StructFieldType *> fields;
};

class ADTType : public BaseType, public SubstitutionRef
{
public:
  enum ADTKind
  {
    STRUCT_STRUCT,
    TUPLE_STRUCT,
    UNION,
    ENUM
  };

  ADTType (HirId ref, std::string identifier, RustIdent ident, ADTKind adt_kind,
	   std::vector<VariantDef *> variants,
	   std::vector<SubstitutionParamMapping> subst_refs,
	   SubstitutionArgumentMappings generic_arguments
	   = SubstitutionArgumentMappings::error (),
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ADT, ident, refs),
      SubstitutionRef (std::move (subst_refs), std::move (generic_arguments)),
      identifier (identifier), variants (variants), adt_kind (adt_kind)
  {}

  ADTType (HirId ref, HirId ty_ref, std::string identifier, RustIdent ident,
	   ADTKind adt_kind, std::vector<VariantDef *> variants,
	   std::vector<SubstitutionParamMapping> subst_refs,
	   SubstitutionArgumentMappings generic_arguments
	   = SubstitutionArgumentMappings::error (),
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ADT, ident, refs),
      SubstitutionRef (std::move (subst_refs), std::move (generic_arguments)),
      identifier (identifier), variants (variants), adt_kind (adt_kind)
  {}

  ADTKind get_adt_kind () const { return adt_kind; }

  bool is_struct_struct () const { return adt_kind == STRUCT_STRUCT; }
  bool is_tuple_struct () const { return adt_kind == TUPLE_STRUCT; }
  bool is_union () const { return adt_kind == UNION; }
  bool is_enum () const { return adt_kind == ENUM; }

  bool is_unit () const override
  {
    if (number_of_variants () == 0)
      return true;

    if (number_of_variants () == 1)
      return variants.at (0)->num_fields () == 0;

    return false;
  }

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  std::string get_identifier () const { return identifier; }

  std::string get_name () const override final
  {
    return identifier + subst_as_string ();
  }

  bool is_concrete () const override final
  {
    for (auto &variant : variants)
      {
	for (auto &field : variant->get_fields ())
	  {
	    if (!field->is_concrete ())
	      return false;
	  }
      }
    return true;
  }

  BaseType *clone () const final override;

  bool needs_generic_substitutions () const override final
  {
    return needs_substitution ();
  }

  bool supports_substitutions () const override final { return true; }

  bool has_subsititions_defined () const override final
  {
    return has_substitutions ();
  }

  size_t number_of_variants () const { return variants.size (); }

  std::vector<VariantDef *> &get_variants () { return variants; }
  const std::vector<VariantDef *> &get_variants () const { return variants; }

  bool lookup_variant (const std::string &lookup,
		       VariantDef **found_variant) const
  {
    for (auto &variant : variants)
      {
	if (variant->get_identifier ().compare (lookup) == 0)
	  {
	    *found_variant = variant;
	    return true;
	  }
      }
    return false;
  }

  bool lookup_variant_by_id (HirId id, VariantDef **found_variant,
			     int *index = nullptr) const
  {
    int i = 0;
    for (auto &variant : variants)
      {
	if (variant->get_id () == id)
	  {
	    if (index != nullptr)
	      *index = i;

	    *found_variant = variant;
	    return true;
	  }
	i++;
      }
    return false;
  }

  ADTType *
  handle_substitions (SubstitutionArgumentMappings mappings) override final;

private:
  std::string identifier;
  std::vector<VariantDef *> variants;
  ADTType::ADTKind adt_kind;
};

class FnType : public BaseType, public SubstitutionRef
{
public:
  static const uint8_t FNTYPE_DEFAULT_FLAGS = 0x00;
  static const uint8_t FNTYPE_IS_METHOD_FLAG = 0x01;
  static const uint8_t FNTYPE_IS_EXTERN_FLAG = 0x02;
  static const uint8_t FNTYPE_IS_VARADIC_FLAG = 0X04;

  FnType (HirId ref, DefId id, std::string identifier, RustIdent ident,
	  uint8_t flags, ABI abi,
	  std::vector<std::pair<HIR::Pattern *, BaseType *>> params,
	  BaseType *type, std::vector<SubstitutionParamMapping> subst_refs,
	  std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::FNDEF, ident, refs),
      SubstitutionRef (std::move (subst_refs),
		       SubstitutionArgumentMappings::error ()),
      params (std::move (params)), type (type), flags (flags),
      identifier (identifier), id (id), abi (abi)
  {
    LocalDefId local_def_id = id.localDefId;
    rust_assert (local_def_id != UNKNOWN_LOCAL_DEFID);
  }

  FnType (HirId ref, HirId ty_ref, DefId id, std::string identifier,
	  RustIdent ident, uint8_t flags, ABI abi,
	  std::vector<std::pair<HIR::Pattern *, BaseType *>> params,
	  BaseType *type, std::vector<SubstitutionParamMapping> subst_refs,
	  std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::FNDEF, ident, refs),
      SubstitutionRef (std::move (subst_refs),
		       SubstitutionArgumentMappings::error ()),
      params (params), type (type), flags (flags), identifier (identifier),
      id (id), abi (abi)
  {
    LocalDefId local_def_id = id.localDefId;
    rust_assert (local_def_id != UNKNOWN_LOCAL_DEFID);
  }

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  std::string get_identifier () const { return identifier; }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  size_t num_params () const { return params.size (); }

  bool is_method () const
  {
    if (num_params () == 0)
      return false;

    return (flags & FNTYPE_IS_METHOD_FLAG) != 0;
  }

  bool is_extern () const { return (flags & FNTYPE_IS_EXTERN_FLAG) != 0; }

  bool is_varadic () const { return (flags & FNTYPE_IS_VARADIC_FLAG) != 0; }

  DefId get_id () const { return id; }

  // get the Self type for the method
  BaseType *get_self_type () const
  {
    rust_assert (is_method ());
    return param_at (0).second;
  }

  bool is_concrete () const override final
  {
    for (const auto &param : params)
      {
	const BaseType *p = param.second;
	if (!p->is_concrete ())
	  return false;
      }
    return get_return_type ()->is_concrete ();
  }

  std::vector<std::pair<HIR::Pattern *, BaseType *>> &get_params ()
  {
    return params;
  }

  const std::vector<std::pair<HIR::Pattern *, BaseType *>> &get_params () const
  {
    return params;
  }

  std::pair<HIR::Pattern *, BaseType *> &param_at (size_t idx)
  {
    return params.at (idx);
  }

  const std::pair<HIR::Pattern *, BaseType *> &param_at (size_t idx) const
  {
    return params.at (idx);
  }

  BaseType *get_return_type () const { return type; }

  BaseType *clone () const final override;

  bool needs_generic_substitutions () const override final
  {
    return needs_substitution ();
  }

  bool supports_substitutions () const override final { return true; }

  bool has_subsititions_defined () const override final
  {
    return has_substitutions ();
  }

  FnType *
  handle_substitions (SubstitutionArgumentMappings mappings) override final;

  ABI get_abi () const { return abi; }

private:
  std::vector<std::pair<HIR::Pattern *, BaseType *>> params;
  BaseType *type;
  uint8_t flags;
  std::string identifier;
  DefId id;
  ABI abi;
};

class FnPtr : public BaseType
{
public:
  FnPtr (HirId ref, Location locus, std::vector<TyVar> params,
	 TyVar result_type, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::FNPTR,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      params (std::move (params)), result_type (result_type)
  {}

  FnPtr (HirId ref, HirId ty_ref, Location locus, std::vector<TyVar> params,
	 TyVar result_type, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::FNPTR,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      params (params), result_type (result_type)
  {}

  std::string get_name () const override final { return as_string (); }

  BaseType *get_return_type () const { return result_type.get_tyty (); }

  size_t num_params () const { return params.size (); }

  BaseType *param_at (size_t idx) const { return params.at (idx).get_tyty (); }

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;

  void iterate_params (std::function<bool (BaseType *)> cb) const
  {
    for (auto &p : params)
      {
	if (!cb (p.get_tyty ()))
	  return;
      }
  }

  std::vector<TyVar> &get_params () { return params; }
  const std::vector<TyVar> &get_params () const { return params; }

  bool is_concrete () const override final
  {
    for (auto &p : params)
      {
	if (!p.get_tyty ()->is_concrete ())
	  return false;
      }
    return result_type.get_tyty ()->is_concrete ();
  }

private:
  std::vector<TyVar> params;
  TyVar result_type;
};

class ClosureType : public BaseType, public SubstitutionRef
{
public:
  ClosureType (HirId ref, DefId id, RustIdent ident,
	       std::vector<TyVar> parameter_types, TyVar result_type,
	       std::vector<SubstitutionParamMapping> subst_refs,
	       std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::CLOSURE, ident, refs),
      SubstitutionRef (std::move (subst_refs),
		       SubstitutionArgumentMappings::error ()),
      parameter_types (std::move (parameter_types)),
      result_type (std::move (result_type)), id (id)
  {
    LocalDefId local_def_id = id.localDefId;
    rust_assert (local_def_id != UNKNOWN_LOCAL_DEFID);
  }

  ClosureType (HirId ref, HirId ty_ref, RustIdent ident, DefId id,
	       std::vector<TyVar> parameter_types, TyVar result_type,
	       std::vector<SubstitutionParamMapping> subst_refs,
	       std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::CLOSURE, ident, refs),
      SubstitutionRef (std::move (subst_refs),
		       SubstitutionArgumentMappings::error ()),
      parameter_types (std::move (parameter_types)),
      result_type (std::move (result_type)), id (id)
  {
    LocalDefId local_def_id = id.localDefId;
    rust_assert (local_def_id != UNKNOWN_LOCAL_DEFID);
  }

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;
  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;

  bool is_concrete () const override final
  {
    for (auto &param : parameter_types)
      {
	auto p = param.get_tyty ();
	if (!p->is_concrete ())
	  return false;
      }
    return result_type.get_tyty ()->is_concrete ();
  }

  bool needs_generic_substitutions () const override final
  {
    return needs_substitution ();
  }

  bool supports_substitutions () const override final { return true; }

  bool has_subsititions_defined () const override final
  {
    return has_substitutions ();
  }

  ClosureType *
  handle_substitions (SubstitutionArgumentMappings mappings) override final;

private:
  std::vector<TyVar> parameter_types;
  TyVar result_type;
  DefId id;
};

class ArrayType : public BaseType
{
public:
  ArrayType (HirId ref, Location locus, HIR::Expr &capacity_expr, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ARRAY,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      element_type (base), capacity_expr (capacity_expr)
  {}

  ArrayType (HirId ref, HirId ty_ref, Location locus, HIR::Expr &capacity_expr,
	     TyVar base, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ARRAY,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      element_type (base), capacity_expr (capacity_expr)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  BaseType *get_element_type () const;

  BaseType *clone () const final override;

  bool is_concrete () const final override
  {
    return get_element_type ()->is_concrete ();
  }

  HIR::Expr &get_capacity_expr () const { return capacity_expr; }

  ArrayType *handle_substitions (SubstitutionArgumentMappings mappings);

private:
  TyVar element_type;
  HIR::Expr &capacity_expr;
};

class SliceType : public BaseType
{
public:
  SliceType (HirId ref, Location locus, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::SLICE,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      element_type (base)
  {}

  SliceType (HirId ref, HirId ty_ref, Location locus, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::SLICE,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      element_type (base)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  BaseType *get_element_type () const;

  BaseType *clone () const final override;

  bool is_concrete () const final override
  {
    return get_element_type ()->is_concrete ();
  }

  SliceType *handle_substitions (SubstitutionArgumentMappings mappings);

private:
  TyVar element_type;
};

class BoolType : public BaseType
{
public:
  BoolType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::BOOL,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  BoolType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::BOOL,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  BaseType *clone () const final override;
  bool is_concrete () const override final { return true; }
};

class IntType : public BaseType
{
public:
  enum IntKind
  {
    I8,
    I16,
    I32,
    I64,
    I128
  };

  IntType (HirId ref, IntKind kind, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::INT,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      int_kind (kind)
  {}

  IntType (HirId ref, HirId ty_ref, IntKind kind,
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::INT,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      int_kind (kind)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  IntKind get_int_kind () const { return int_kind; }

  BaseType *clone () const final override;

  bool is_equal (const BaseType &other) const override;
  bool is_concrete () const override final { return true; }

private:
  IntKind int_kind;
};

class UintType : public BaseType
{
public:
  enum UintKind
  {
    U8,
    U16,
    U32,
    U64,
    U128
  };

  UintType (HirId ref, UintKind kind, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::UINT,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      uint_kind (kind)
  {}

  UintType (HirId ref, HirId ty_ref, UintKind kind,
	    std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::UINT,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      uint_kind (kind)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  UintKind get_uint_kind () const { return uint_kind; }

  BaseType *clone () const final override;

  bool is_equal (const BaseType &other) const override;
  bool is_concrete () const override final { return true; }

private:
  UintKind uint_kind;
};

class FloatType : public BaseType
{
public:
  enum FloatKind
  {
    F32,
    F64
  };

  FloatType (HirId ref, FloatKind kind,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::FLOAT,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      float_kind (kind)
  {}

  FloatType (HirId ref, HirId ty_ref, FloatKind kind,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::FLOAT,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      float_kind (kind)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  FloatKind get_float_kind () const { return float_kind; }

  BaseType *clone () const final override;

  bool is_equal (const BaseType &other) const override;
  bool is_concrete () const override final { return true; }

private:
  FloatKind float_kind;
};

class USizeType : public BaseType
{
public:
  USizeType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::USIZE,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  USizeType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::USIZE,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  BaseType *clone () const final override;
  bool is_concrete () const override final { return true; }
};

class ISizeType : public BaseType
{
public:
  ISizeType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ISIZE,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  ISizeType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ISIZE,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  BaseType *clone () const final override;
  bool is_concrete () const override final { return true; }
};

class CharType : public BaseType
{
public:
  CharType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::CHAR,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  CharType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::CHAR,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  BaseType *clone () const final override;
  bool is_concrete () const override final { return true; }
};

class ReferenceType : public BaseType
{
public:
  ReferenceType (HirId ref, TyVar base, Mutability mut,
		 std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::REF,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      base (base), mut (mut)
  {}

  ReferenceType (HirId ref, HirId ty_ref, TyVar base, Mutability mut,
		 std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::REF,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      base (base), mut (mut)
  {}

  BaseType *get_base () const;

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final
  {
    return "&" + get_base ()->get_name ();
  }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;

  bool is_concrete () const override final
  {
    return get_base ()->is_concrete ();
  }

  ReferenceType *handle_substitions (SubstitutionArgumentMappings mappings);

  Mutability mutability () const { return mut; }

  bool is_mutable () const { return mut == Mutability::Mut; }

private:
  TyVar base;
  Mutability mut;
};

class PointerType : public BaseType
{
public:
  PointerType (HirId ref, TyVar base, Mutability mut,
	       std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::POINTER,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      base (base), mut (mut)
  {}

  PointerType (HirId ref, HirId ty_ref, TyVar base, Mutability mut,
	       std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::POINTER,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      base (base), mut (mut)
  {}

  BaseType *get_base () const;

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final
  {
    return "*" + get_base ()->get_name ();
  }

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;

  bool is_concrete () const override final
  {
    return get_base ()->is_concrete ();
  }

  PointerType *handle_substitions (SubstitutionArgumentMappings mappings);

  Mutability mutability () const { return mut; }

  bool is_mutable () const { return mut == Mutability::Mut; }

  bool is_const () const { return mut == Mutability::Imm; }

private:
  TyVar base;
  Mutability mut;
};

class StrType : public BaseType
{
public:
  StrType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::STR,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  StrType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::STR,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  std::string get_name () const override final { return as_string (); }

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;
  bool is_concrete () const override final { return true; }
};

// https://doc.rust-lang.org/std/primitive.never.html
//
// Since the `!` type is really complicated and it is even still unstable
// in rustc, only fairly limited support for this type is introduced here.
// Unification between `!` and ANY other type (including `<T?>`) is simply
// not allowed. If it is needed, it should be handled manually. For example,
// unifying `!` with other types is very necessary when resolving types of
// `if/else` expressions.
//
// See related discussion at https://github.com/Rust-GCC/gccrs/pull/364
class NeverType : public BaseType
{
public:
  NeverType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::NEVER,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  NeverType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::NEVER,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  BaseType *clone () const final override;

  std::string get_name () const override final { return as_string (); }

  bool is_unit () const override { return true; }
  bool is_concrete () const override final { return true; }
};

// used at the type in associated types in traits
// see: https://doc.rust-lang.org/book/ch19-03-advanced-traits.html
class PlaceholderType : public BaseType
{
public:
  PlaceholderType (std::string symbol, HirId ref,
		   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::PLACEHOLDER,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      symbol (symbol)
  {}

  PlaceholderType (std::string symbol, HirId ref, HirId ty_ref,
		   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::PLACEHOLDER,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      symbol (symbol)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  BaseType *clone () const final override;

  std::string get_name () const override final { return as_string (); }

  bool is_unit () const override
  {
    rust_assert (can_resolve ());
    return resolve ()->is_unit ();
  }

  std::string get_symbol () const { return symbol; }

  void set_associated_type (HirId ref);

  void clear_associated_type ();

  bool can_resolve () const;

  BaseType *resolve () const;

  bool is_equal (const BaseType &other) const override;

  bool is_concrete () const override final
  {
    if (!can_resolve ())
      return true;

    return resolve ()->is_concrete ();
  }

private:
  std::string symbol;
};

class ProjectionType : public BaseType, public SubstitutionRef
{
public:
  ProjectionType (HirId ref, BaseType *base, Resolver::TraitReference *trait,
		  DefId item, std::vector<SubstitutionParamMapping> subst_refs,
		  SubstitutionArgumentMappings generic_arguments
		  = SubstitutionArgumentMappings::error (),
		  std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::PROJECTION,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      SubstitutionRef (std::move (subst_refs), std::move (generic_arguments)),
      base (base), trait (trait), item (item)
  {}

  ProjectionType (HirId ref, HirId ty_ref, BaseType *base,
		  Resolver::TraitReference *trait, DefId item,
		  std::vector<SubstitutionParamMapping> subst_refs,
		  SubstitutionArgumentMappings generic_arguments
		  = SubstitutionArgumentMappings::error (),
		  std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::PROJECTION,
		{Resolver::CanonicalPath::create_empty (),
		 Linemap::predeclared_location ()},
		refs),
      SubstitutionRef (std::move (subst_refs), std::move (generic_arguments)),
      base (base), trait (trait), item (item)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  BaseType *clone () const final override;

  std::string get_name () const override final { return as_string (); }

  bool is_unit () const override { return false; }

  bool needs_generic_substitutions () const override final
  {
    return needs_substitution ();
  }

  bool supports_substitutions () const override final { return true; }

  bool has_subsititions_defined () const override final
  {
    return has_substitutions ();
  }

  const BaseType *get () const { return base; }
  BaseType *get () { return base; }

  bool is_concrete () const override final { return base->is_concrete (); }

  ProjectionType *
  handle_substitions (SubstitutionArgumentMappings mappings) override final;

private:
  BaseType *base;
  Resolver::TraitReference *trait;
  DefId item;
};

class DynamicObjectType : public BaseType
{
public:
  DynamicObjectType (HirId ref, RustIdent ident,
		     std::vector<TypeBoundPredicate> specified_bounds,
		     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::DYNAMIC, ident, specified_bounds, refs)
  {}

  DynamicObjectType (HirId ref, HirId ty_ref, RustIdent ident,
		     std::vector<TypeBoundPredicate> specified_bounds,
		     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::DYNAMIC, ident, specified_bounds, refs)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;
  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;

  std::string get_name () const override final;

  bool is_concrete () const override final { return true; }

  // this returns a flat list of items including super trait bounds
  const std::vector<
    std::pair<const Resolver::TraitItemReference *, const TypeBoundPredicate *>>
  get_object_items () const;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY
