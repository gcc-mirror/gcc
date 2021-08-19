// Copyright (C) 2020 Free Software Foundation, Inc.

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

#include "rust-backend.h"
#include "rust-hir-map.h"
#include "rust-hir-full.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace Resolver {
class TraitReference;
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
  // there are more to add...
  ERROR
};

class TypeKindFormat
{
public:
  static std::string to_string (TypeKind kind)
  {
    switch (kind)
      {
      case TypeKind::INFER:
	return "Infer";

      case TypeKind::ADT:
	return "ADT";

      case TypeKind::STR:
	return "STR";

      case TypeKind::REF:
	return "REF";

      case TypeKind::POINTER:
	return "POINTER";

      case TypeKind::PARAM:
	return "PARAM";

      case TypeKind::ARRAY:
	return "ARRAY";

      case TypeKind::FNDEF:
	return "FnDef";

      case TypeKind::FNPTR:
	return "FnPtr";

      case TypeKind::TUPLE:
	return "Tuple";

      case TypeKind::BOOL:
	return "Bool";

      case TypeKind::CHAR:
	return "Char";

      case TypeKind::INT:
	return "Int";

      case TypeKind::UINT:
	return "Uint";

      case TypeKind::FLOAT:
	return "Float";

      case TypeKind::USIZE:
	return "Usize";

      case TypeKind::ISIZE:
	return "Isize";

      case TypeKind::NEVER:
	return "Never";

      case TypeKind::PLACEHOLDER:
	return "Placeholder";

      case TypeKind::PROJECTION:
	return "Projection";

      case TypeKind::ERROR:
	return "ERROR";
      }
    gcc_unreachable ();
  }
};

class TypeBoundPredicate
{
public:
  TypeBoundPredicate (DefId reference, Location locus)
    : reference (reference), locus (locus)
  {}

  std::string as_string () const;

  const Resolver::TraitReference *get () const;

  Location get_locus () const { return locus; }

  std::string get_name () const;

private:
  DefId reference;
  Location locus;
};

class TypeBoundsMappings
{
protected:
  TypeBoundsMappings (std::vector<TypeBoundPredicate> specified_bounds)
    : specified_bounds (specified_bounds)
  {}

public:
  std::vector<TypeBoundPredicate> &get_specified_bounds ()
  {
    return specified_bounds;
  }

  const std::vector<TypeBoundPredicate> &get_specified_bounds () const
  {
    return specified_bounds;
  }

  std::string bounds_as_string () const
  {
    std::string buf;
    for (auto &b : specified_bounds)
      buf += b.as_string () + ", ";

    return "bounds:[" + buf + "]";
  }

protected:
  void add_bound (TypeBoundPredicate predicate)
  {
    specified_bounds.push_back (predicate);
  }

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
  // releasing the memory of the returned ty. using ignore_errors alows for a
  // can_eq style unification
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

  bool bounds_compatible (const BaseType &other, Location locus) const;

  void inherit_bounds (const BaseType &other);

  virtual bool is_unit () const { return false; }

  virtual bool is_concrete () const { return true; }

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

  virtual bool contains_type_parameters () const { return false; }

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
	   + mappings_str ();
  }

  void debug () const
  {
    rust_debug ("[%p] %s", static_cast<const void *> (this),
		debug_str ().c_str ());
  }

protected:
  BaseType (HirId ref, HirId ty_ref, TypeKind kind,
	    std::set<HirId> refs = std::set<HirId> ())
    : TypeBoundsMappings ({}), kind (kind), ref (ref), ty_ref (ty_ref),
      combined (refs), mappings (Analysis::Mappings::get ())
  {}

  BaseType (HirId ref, HirId ty_ref, TypeKind kind,
	    std::vector<TypeBoundPredicate> specified_bounds,
	    std::set<HirId> refs = std::set<HirId> ())
    : TypeBoundsMappings (specified_bounds), kind (kind), ref (ref),
      ty_ref (ty_ref), combined (refs), mappings (Analysis::Mappings::get ())
  {}

  TypeKind kind;
  HirId ref;
  HirId ty_ref;
  std::set<HirId> combined;

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

  InferType (HirId ref, InferTypeKind infer_kind,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::INFER, refs), infer_kind (infer_kind)
  {}

  InferType (HirId ref, HirId ty_ref, InferTypeKind infer_kind,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::INFER, refs), infer_kind (infer_kind)
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

  bool is_concrete () const final override { return false; }

private:
  InferTypeKind infer_kind;
};

class ErrorType : public BaseType
{
public:
  ErrorType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ERROR, refs)
  {}

  ErrorType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ERROR, refs)
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
};

class SubstitutionArgumentMappings;
class ParamType : public BaseType
{
public:
  ParamType (std::string symbol, HirId ref, HIR::GenericParam &param,
	     std::vector<TypeBoundPredicate> specified_bounds,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::PARAM, specified_bounds, refs),
      symbol (symbol), param (param)
  {}

  ParamType (std::string symbol, HirId ref, HirId ty_ref,
	     HIR::GenericParam &param,
	     std::vector<TypeBoundPredicate> specified_bounds,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::PARAM, specified_bounds, refs),
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

  std::string get_name () const override final { return as_string (); }

  bool is_equal (const BaseType &other) const override;

  bool contains_type_parameters () const override final
  {
    if (can_resolve ())
      {
	auto r = resolve ();
	return r->contains_type_parameters ();
      }
    return true;
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

  void debug () const { rust_debug ("%s", as_string ().c_str ()); }

private:
  HirId ref;
  std::string name;
  BaseType *ty;
};

class TupleType : public BaseType
{
public:
  TupleType (HirId ref, std::vector<TyVar> fields = std::vector<TyVar> (),
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::TUPLE, refs), fields (fields)
  {}

  TupleType (HirId ref, HirId ty_ref,
	     std::vector<TyVar> fields = std::vector<TyVar> (),
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::TUPLE, refs), fields (fields)
  {}

  static TupleType *get_unit_type (HirId ref) { return new TupleType (ref); }

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

  void iterate_fields (std::function<bool (BaseType *)> cb) const
  {
    for (size_t i = 0; i < num_fields (); i++)
      {
	if (!cb (get_field (i)))
	  return;
      }
  }

  std::string get_name () const override final { return as_string (); }

  bool contains_type_parameters () const override final
  {
    for (auto &f : fields)
      {
	if (f.get_tyty ()->contains_type_parameters ())
	  return true;
      }
    return false;
  }

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

  std::string as_string () const { return param->as_string (); }

  void fill_param_ty (BaseType *type, Location locus)
  {
    if (type->get_kind () == TyTy::TypeKind::INFER)
      {
	type->inherit_bounds (*param);
      }
    else
      {
	if (!param->bounds_compatible (*type, locus))
	  return;
      }

    if (type->get_kind () == TypeKind::PARAM)
      {
	delete param;
	param = static_cast<ParamType *> (type->clone ());
      }
    else
      {
	param->set_ty_ref (type->get_ref ());
      }
  }

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

private:
  const HIR::TypeParam &generic;
  ParamType *param;
};

class SubstitutionArg
{
public:
  SubstitutionArg (SubstitutionParamMapping *param, BaseType *argument)
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

  SubstitutionParamMapping *get_param_mapping () { return param; }

  static SubstitutionArg error () { return SubstitutionArg (nullptr, nullptr); }

  bool is_error () const { return param == nullptr || argument == nullptr; }

  bool is_conrete () const
  {
    return argument != nullptr && argument->get_kind () != TyTy::TypeKind::ERROR
	   && argument->get_kind () != TyTy::TypeKind::PARAM;
  }

  std::string as_string () const
  {
    return param->as_string () + ":" + argument->as_string ();
  }

private:
  SubstitutionParamMapping *param;
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
	SubstitutionParamMapping *param = mapping.get_param_mapping ();
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

  Location get_locus () { return locus; }

  size_t size () const { return mappings.size (); }

  std::vector<SubstitutionArg> &get_mappings () { return mappings; }

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
    if (!has_substitutions ())
      return false;

    if (used_arguments.is_error ())
      return true;

    if (used_arguments.size () != get_num_substitutions ())
      return true;

    return !used_arguments.is_concrete ();
  }

  bool was_substituted () const { return !needs_substitution (); }

  SubstitutionArgumentMappings get_substitution_arguments ()
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
    SubstitutionArgumentMappings &mappings);

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

protected:
  std::vector<SubstitutionParamMapping> substitutions;
  SubstitutionArgumentMappings used_arguments;
};

class ADTType : public BaseType, public SubstitutionRef
{
public:
  enum ADTKind
  {
    STRUCT_STRUCT,
    TUPLE_STRUCT,
    UNION,
    // ENUM ?
  };

  ADTType (HirId ref, std::string identifier, ADTKind adt_kind,
	   std::vector<StructFieldType *> fields,
	   std::vector<SubstitutionParamMapping> subst_refs,
	   SubstitutionArgumentMappings generic_arguments
	   = SubstitutionArgumentMappings::error (),
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ADT, refs),
      SubstitutionRef (std::move (subst_refs), std::move (generic_arguments)),
      identifier (identifier), fields (fields), adt_kind (adt_kind)
  {}

  ADTType (HirId ref, HirId ty_ref, std::string identifier, ADTKind adt_kind,
	   std::vector<StructFieldType *> fields,
	   std::vector<SubstitutionParamMapping> subst_refs,
	   SubstitutionArgumentMappings generic_arguments
	   = SubstitutionArgumentMappings::error (),
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ADT, refs),
      SubstitutionRef (std::move (subst_refs), std::move (generic_arguments)),
      identifier (identifier), fields (fields), adt_kind (adt_kind)
  {}

  ADTKind get_adt_kind () const { return adt_kind; }
  bool is_tuple_struct () const { return adt_kind == TUPLE_STRUCT; }
  bool is_union () const { return adt_kind == UNION; }

  bool is_unit () const override { return this->fields.empty (); }

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;
  bool can_eq (const BaseType *other, bool emit_errors) const override final;
  BaseType *coerce (BaseType *other) override;
  BaseType *cast (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  size_t num_fields () const { return fields.size (); }

  std::string get_identifier () const { return identifier; }

  std::string get_name () const override final
  {
    return identifier + subst_as_string ();
  }

  BaseType *get_field_type (size_t index);

  const BaseType *get_field_type (size_t index) const;

  const StructFieldType *get_field (size_t index) const;

  StructFieldType *get_field (size_t index) { return fields.at (index); }

  const StructFieldType *get_imm_field (size_t index) const
  {
    return fields.at (index);
  }

  StructFieldType *get_field (const std::string &lookup,
			      size_t *index = nullptr) const
  {
    size_t i = 0;
    for (auto &field : fields)
      {
	if (field->get_name ().compare (lookup) == 0)
	  {
	    if (index != nullptr)
	      *index = i;
	    return field;
	  }
	i++;
      }
    return nullptr;
  }

  BaseType *clone () const final override;

  std::vector<StructFieldType *> &get_fields () { return fields; }
  const std::vector<StructFieldType *> &get_fields () const { return fields; }

  void iterate_fields (std::function<bool (StructFieldType *)> cb)
  {
    for (auto &f : fields)
      {
	if (!cb (f))
	  return;
      }
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

  ADTType *
  handle_substitions (SubstitutionArgumentMappings mappings) override final;

private:
  std::string identifier;
  std::vector<StructFieldType *> fields;
  ADTType::ADTKind adt_kind;
};

class FnType : public BaseType, public SubstitutionRef
{
public:
#define FNTYPE_DEFAULT_FLAGS 0x00
#define FNTYPE_IS_METHOD_FLAG 0x01
#define FNTYPE_IS_EXTERN_FLAG 0x02
#define FNTYPE_IS_VARADIC_FLAG 0X04

  FnType (HirId ref, DefId id, std::string identifier, uint8_t flags,
	  std::vector<std::pair<HIR::Pattern *, BaseType *>> params,
	  BaseType *type, std::vector<SubstitutionParamMapping> subst_refs,
	  std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::FNDEF, refs),
      SubstitutionRef (std::move (subst_refs),
		       SubstitutionArgumentMappings::error ()),
      params (std::move (params)), type (type), flags (flags),
      identifier (identifier), id (id)
  {
    LocalDefId local_def_id = id & DEF_ID_LOCAL_DEF_MASK;
    rust_assert (local_def_id != UNKNOWN_LOCAL_DEFID);
  }

  FnType (HirId ref, HirId ty_ref, DefId id, std::string identifier,
	  uint8_t flags,
	  std::vector<std::pair<HIR::Pattern *, BaseType *>> params,
	  BaseType *type, std::vector<SubstitutionParamMapping> subst_refs,
	  std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::FNDEF, refs),
      SubstitutionRef (std::move (subst_refs),
		       SubstitutionArgumentMappings::error ()),
      params (params), type (type), flags (flags), identifier (identifier),
      id (id)
  {
    LocalDefId local_def_id = id & DEF_ID_LOCAL_DEF_MASK;
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
    // FIXME this will need updated when we support coercion for & mut self etc
    return get_params ().at (0).second;
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

private:
  std::vector<std::pair<HIR::Pattern *, BaseType *>> params;
  BaseType *type;
  uint8_t flags;
  std::string identifier;
  DefId id;
};

class FnPtr : public BaseType
{
public:
  FnPtr (HirId ref, std::vector<TyVar> params, TyVar result_type,
	 std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::FNPTR, refs), params (std::move (params)),
      result_type (result_type)
  {}

  FnPtr (HirId ref, HirId ty_ref, std::vector<TyVar> params, TyVar result_type,
	 std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::FNPTR, refs), params (params),
      result_type (result_type)
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

private:
  std::vector<TyVar> params;
  TyVar result_type;
};

class ArrayType : public BaseType
{
public:
  ArrayType (HirId ref, Bexpression *capacity, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ARRAY, refs), capacity (capacity),
      element_type (base)
  {}

  ArrayType (HirId ref, HirId ty_ref, Bexpression *capacity, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ARRAY, refs), capacity (capacity),
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

  Bexpression *get_capacity () const { return capacity; }
  std::string capacity_string () const;

  BaseType *get_element_type () const;

  BaseType *clone () const final override;

  bool is_concrete () const final override
  {
    return get_element_type ()->is_concrete ();
  }

private:
  Bexpression *capacity;
  TyVar element_type;
};

class BoolType : public BaseType
{
public:
  BoolType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::BOOL, refs)
  {}

  BoolType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::BOOL, refs)
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
    : BaseType (ref, ref, TypeKind::INT, refs), int_kind (kind)
  {}

  IntType (HirId ref, HirId ty_ref, IntKind kind,
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::INT, refs), int_kind (kind)
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
    : BaseType (ref, ref, TypeKind::UINT, refs), uint_kind (kind)
  {}

  UintType (HirId ref, HirId ty_ref, UintKind kind,
	    std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::UINT, refs), uint_kind (kind)
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
    : BaseType (ref, ref, TypeKind::FLOAT, refs), float_kind (kind)
  {}

  FloatType (HirId ref, HirId ty_ref, FloatKind kind,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::FLOAT, refs), float_kind (kind)
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

private:
  FloatKind float_kind;
};

class USizeType : public BaseType
{
public:
  USizeType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::USIZE, refs)
  {}

  USizeType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::USIZE, refs)
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
};

class ISizeType : public BaseType
{
public:
  ISizeType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ISIZE, refs)
  {}

  ISizeType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ISIZE, refs)
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
};

class CharType : public BaseType
{
public:
  CharType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::CHAR, refs)
  {}

  CharType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::CHAR, refs)
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
};

class ReferenceType : public BaseType
{
public:
  ReferenceType (HirId ref, TyVar base, bool is_mut,
		 std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::REF, refs), base (base), is_mut (is_mut)
  {}

  ReferenceType (HirId ref, HirId ty_ref, TyVar base, bool is_mut,
		 std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::REF, refs), base (base), is_mut (is_mut)
  {}

  BaseType *get_base () const;

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

  bool contains_type_parameters () const override final
  {
    return get_base ()->contains_type_parameters ();
  }

  ReferenceType *handle_substitions (SubstitutionArgumentMappings mappings);

  bool is_mutable () const { return is_mut; }

private:
  TyVar base;
  bool is_mut;
};

class PointerType : public BaseType
{
public:
  PointerType (HirId ref, TyVar base, bool is_mut,
	       std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::POINTER, refs), base (base), is_mut (is_mut)
  {}

  PointerType (HirId ref, HirId ty_ref, TyVar base, bool is_mut,
	       std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::POINTER, refs), base (base),
      is_mut (is_mut)
  {}

  BaseType *get_base () const;

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

  bool contains_type_parameters () const override final
  {
    return get_base ()->contains_type_parameters ();
  }

  PointerType *handle_substitions (SubstitutionArgumentMappings mappings);

  bool is_mutable () const { return is_mut; }

  bool is_const () const { return !is_mut; }

private:
  TyVar base;
  bool is_mut;
};

class StrType : public BaseType
{
public:
  StrType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::STR, refs)
  {}

  StrType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::STR, refs)
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
    : BaseType (ref, ref, TypeKind::NEVER, refs)
  {}

  NeverType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::NEVER, refs)
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
};

// used at the type in associated types in traits
// see: https://doc.rust-lang.org/book/ch19-03-advanced-traits.html
class PlaceholderType : public BaseType
{
public:
  PlaceholderType (std::string symbol, HirId ref,
		   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::PLACEHOLDER, refs), symbol (symbol)

  {}

  PlaceholderType (std::string symbol, HirId ref, HirId ty_ref,
		   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::PLACEHOLDER, refs), symbol (symbol)
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

  std::string get_symbol () const { return symbol; }

  void set_associated_type (HirId ref);

  void clear_associated_type ();

  bool can_resolve () const;

  BaseType *resolve () const;

  bool is_equal (const BaseType &other) const override;

private:
  std::string symbol;
};

class ProjectionType : public BaseType
{
public:
  ProjectionType (HirId ref, TyVar base, Resolver::TraitReference *trait,
		  DefId item, Resolver::AssociatedImplTrait *associated,
		  std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::PROJECTION, refs), base (base),
      trait (trait), item (item), associated (associated)
  {}

  ProjectionType (HirId ref, HirId ty_ref, TyVar base,
		  Resolver::TraitReference *trait, DefId item,
		  Resolver::AssociatedImplTrait *associated,
		  std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::PROJECTION, refs), base (base),
      trait (trait), item (item), associated (associated)
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

private:
  TyVar base;
  Resolver::TraitReference *trait;
  DefId item;
  Resolver::AssociatedImplTrait *associated;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY
