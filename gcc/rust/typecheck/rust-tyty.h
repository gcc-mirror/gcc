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

#ifndef RUST_TYTY
#define RUST_TYTY

#include "rust-hir-map.h"
#include "rust-common.h"
#include "rust-identifier.h"
#include "rust-abi.h"
#include "rust-tyty-bounds.h"
#include "rust-tyty-util.h"
#include "rust-tyty-subst.h"
#include "rust-tyty-region.h"
#include "rust-system.h"

namespace Rust {

namespace Resolver {
class TraitReference;

class TraitItemReference;

class AssociatedImplTrait;
} // namespace Resolver

namespace TyTy {
class ClosureType;
class FnPtr;
class FnType;
class CallableTypeInterface;

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

extern bool
is_primitive_type_kind (TypeKind kind);

class TypeKindFormat
{
public:
  static std::string to_string (TypeKind kind);
};

class TyVisitor;
class TyConstVisitor;
class BaseType : public TypeBoundsMappings
{
public:
  virtual ~BaseType ();

  HirId get_ref () const;
  void set_ref (HirId id);

  HirId get_ty_ref () const;
  void set_ty_ref (HirId id);

  HirId get_orig_ref () const;

  virtual void accept_vis (TyVisitor &vis) = 0;
  virtual void accept_vis (TyConstVisitor &vis) const = 0;

  virtual std::string as_string () const = 0;
  virtual std::string get_name () const = 0;

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

  // Check value equality between two ty. Type inference rules are ignored. Two
  //   ty are considered equal if they're of the same kind, and
  //     1. (For ADTs, arrays, tuples, refs) have the same underlying ty
  //     2. (For functions) have the same signature
  virtual bool is_equal (const BaseType &other) const;

  bool satisfies_bound (const TypeBoundPredicate &predicate,
			bool emit_error) const;

  bool bounds_compatible (const BaseType &other, location_t locus,
			  bool emit_error) const;

  void inherit_bounds (const BaseType &other);

  void inherit_bounds (
    const std::vector<TyTy::TypeBoundPredicate> &specified_bounds);

  // is_unit returns whether this is just a unit-struct
  bool is_unit () const;

  // is_concrete returns true if the type is fully resolved to concrete
  // primitives
  bool is_concrete () const;

  // return the type-kind
  TypeKind get_kind () const;

  // monomorphized clone is a clone which destructures the types to get rid of
  // generics
  BaseType *monomorphized_clone () const;

  // get_combined_refs returns the chain of node refs involved in unification
  std::set<HirId> get_combined_refs () const;

  void append_reference (HirId id);

  std::string mappings_str () const;

  std::string debug_str () const;

  void debug () const;

  // FIXME this will eventually go away
  const BaseType *get_root () const;

  // This will get the monomorphized type from Params, Placeholders or
  // Projections if available or error
  BaseType *destructure ();
  const BaseType *destructure () const;

  const RustIdent &get_ident () const;
  location_t get_locus () const;

  bool has_substitutions_defined () const;
  bool needs_generic_substitutions () const;

  std::string mangle_string () const
  {
    return TypeKindFormat::to_string (get_kind ()) + ":" + as_string () + ":"
	   + mappings_str () + ":" + bounds_as_string ();
  }

  /* Returns a pointer to a clone of this. The caller is responsible for
   * releasing the memory of the returned ty. */
  virtual BaseType *clone () const = 0;

  // Check if TyTy::BaseType is of a specific type.
  template <typename T> WARN_UNUSED_RESULT bool is () const
  {
    static_assert (std::is_base_of<BaseType, T>::value,
		   "Can only safely cast to TyTy types.");
    return this->get_kind () == T::KIND;
  }

  template <typename T> T *as () const
  {
    static_assert (std::is_base_of<BaseType, T>::value,
		   "Can only safely cast to TyTy types.");
    rust_assert (this->is<T> ());
    return static_cast<T *> (this);
  }

  template <typename T> T *as ()
  {
    static_assert (std::is_base_of<BaseType, T>::value,
		   "Can only safely cast to TyTy types.");
    rust_assert (this->is<T> ());
    return static_cast<T *> (this);
  }

  // Check if TyTy::BaseType is of a specific type and convert it to that type
  // if so.
  // Returns nullptr otherwise. Works as a dynamic_cast, but without compiler
  // RTTI.
  template <typename T> T *try_as () const
  {
    static_assert (std::is_base_of<BaseType, T>::value,
		   "Can only safely cast to TyTy types.");
    if (!this->is<T> ())
      return nullptr;

    return static_cast<T *> (this);
  }

  // See above.
  template <typename T> T *try_as ()
  {
    static_assert (std::is_base_of<BaseType, T>::value,
		   "Can only safely cast to TyTy types.");
    if (!this->is<T> ())
      return nullptr;

    return static_cast<T *> (this);
  }

protected:
  BaseType (HirId ref, HirId ty_ref, TypeKind kind, RustIdent ident,
	    std::set<HirId> refs = std::set<HirId> ());

  BaseType (HirId ref, HirId ty_ref, TypeKind kind, RustIdent ident,
	    std::vector<TypeBoundPredicate> specified_bounds,
	    std::set<HirId> refs = std::set<HirId> ());

  TypeKind kind;
  HirId ref;
  HirId ty_ref;
  const HirId orig_ref;
  std::set<HirId> combined;
  RustIdent ident;

  Analysis::Mappings *mappings;
};

/** Unified interface for all function-like types. */
class CallableTypeInterface : public BaseType
{
public:
  explicit CallableTypeInterface (HirId ref, HirId ty_ref, TypeKind kind,
				  RustIdent ident,
				  std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, kind, ident, refs)
  {}

  WARN_UNUSED_RESULT virtual size_t get_num_params () const = 0;
  WARN_UNUSED_RESULT virtual BaseType *
  get_param_type_at (size_t index) const = 0;
  WARN_UNUSED_RESULT virtual BaseType *get_return_type () const = 0;
};

class InferType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::INFER;

  enum InferTypeKind
  {
    GENERAL,
    INTEGRAL,
    FLOAT
  };

  struct TypeHint
  {
    enum SignedHint
    {
      SIGNED,
      UNSIGNED,

      UNKNOWN
    };
    enum SizeHint
    {
      S8,
      S16,
      S32,
      S64,
      S128,
      SUNKNOWN
    };

    TyTy::TypeKind kind;
    SignedHint shint;
    SizeHint szhint;

    static TypeHint Default ()
    {
      return TypeHint{TypeKind::ERROR, UNKNOWN, SUNKNOWN};
    }
  };

  InferType (HirId ref, InferTypeKind infer_kind, TypeHint hint,
	     location_t locus, std::set<HirId> refs = std::set<HirId> ());

  InferType (HirId ref, HirId ty_ref, InferTypeKind infer_kind, TypeHint hint,
	     location_t locus, std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;

  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  BaseType *clone () const final override;

  InferTypeKind get_infer_kind () const;

  std::string get_name () const override final;

  bool default_type (BaseType **type) const;

  void apply_primitive_type_hint (const TyTy::BaseType &hint);

private:
  InferTypeKind infer_kind;
  TypeHint default_hint;
};

class ErrorType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::ERROR;

  ErrorType (HirId ref, std::set<HirId> refs = std::set<HirId> ());

  ErrorType (HirId ref, HirId ty_ref,
	     std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  BaseType *clone () const final override;

  std::string get_name () const override final;
};

class ParamType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::PARAM;

  ParamType (std::string symbol, location_t locus, HirId ref,
	     HIR::GenericParam &param,
	     std::vector<TypeBoundPredicate> specified_bounds,
	     std::set<HirId> refs = std::set<HirId> ());

  ParamType (bool is_trait_self, std::string symbol, location_t locus,
	     HirId ref, HirId ty_ref, HIR::GenericParam &param,
	     std::vector<TypeBoundPredicate> specified_bounds,
	     std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  BaseType *clone () const final override;

  std::string get_symbol () const;

  HIR::GenericParam &get_generic_param ();

  bool can_resolve () const;

  BaseType *resolve () const;

  std::string get_name () const override final;

  bool is_equal (const BaseType &other) const override;

  ParamType *handle_substitions (SubstitutionArgumentMappings &mappings);

  void set_implicit_self_trait ();
  bool is_implicit_self_trait () const;

private:
  bool is_trait_self;
  std::string symbol;
  HIR::GenericParam &param;
};

class StructFieldType
{
public:
  StructFieldType (HirId ref, std::string name, BaseType *ty, location_t locus);

  HirId get_ref () const;

  bool is_equal (const StructFieldType &other) const;

  std::string get_name () const;

  BaseType *get_field_type () const;
  void set_field_type (BaseType *fty);

  StructFieldType *clone () const;
  StructFieldType *monomorphized_clone () const;

  void debug () const;
  location_t get_locus () const;
  std::string as_string () const;

private:
  HirId ref;
  std::string name;
  BaseType *ty;
  location_t locus;
};

class TupleType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::TUPLE;

  TupleType (HirId ref, location_t locus,
	     std::vector<TyVar> fields = std::vector<TyVar> (),
	     std::set<HirId> refs = std::set<HirId> ());

  TupleType (HirId ref, HirId ty_ref, location_t locus,
	     std::vector<TyVar> fields = std::vector<TyVar> (),
	     std::set<HirId> refs = std::set<HirId> ());

  static TupleType *get_unit_type (HirId ref);

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  size_t num_fields () const;

  BaseType *get_field (size_t index) const;

  BaseType *clone () const final override;

  const std::vector<TyVar> &get_fields () const;

  std::string get_name () const override final;

  TupleType *handle_substitions (SubstitutionArgumentMappings &mappings);

private:
  std::vector<TyVar> fields;
};

class TypeBoundPredicate : public SubstitutionRef
{
public:
  TypeBoundPredicate (const Resolver::TraitReference &trait_reference,
		      BoundPolarity polarity, location_t locus);

  TypeBoundPredicate (DefId reference,
		      std::vector<SubstitutionParamMapping> substitutions,
		      BoundPolarity polarity, location_t locus);

  TypeBoundPredicate (const TypeBoundPredicate &other);

  virtual ~TypeBoundPredicate (){};

  TypeBoundPredicate &operator= (const TypeBoundPredicate &other);

  static TypeBoundPredicate error ();

  std::string as_string () const;

  std::string as_name () const;

  const Resolver::TraitReference *get () const;

  location_t get_locus () const { return locus; }

  std::string get_name () const;

  // check that this predicate is object-safe see:
  // https://doc.rust-lang.org/reference/items/traits.html#object-safety
  bool is_object_safe (bool emit_error, location_t locus) const;

  void apply_generic_arguments (HIR::GenericArgs *generic_args,
				bool has_associated_self);

  bool contains_item (const std::string &search) const;

  TypeBoundPredicateItem
  lookup_associated_item (const std::string &search) const;

  TypeBoundPredicateItem
  lookup_associated_item (const Resolver::TraitItemReference *ref) const;

  // WARNING THIS WILL ALWAYS RETURN NULLPTR
  BaseType *
  handle_substitions (SubstitutionArgumentMappings &mappings) override final;

  bool is_error () const;

  bool requires_generic_args () const;

  bool contains_associated_types () const;

  DefId get_id () const { return reference; }

  BoundPolarity get_polarity () const { return polarity; }

  std::vector<TypeBoundPredicateItem> get_associated_type_items ();

  size_t get_num_associated_bindings () const override final;

  TypeBoundPredicateItem
  lookup_associated_type (const std::string &search) override final;

  bool is_equal (const TypeBoundPredicate &other) const;

private:
  struct mark_is_error
  {
  };

  TypeBoundPredicate (mark_is_error);

  DefId reference;
  location_t locus;
  bool error_flag;
  BoundPolarity polarity;
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

  static std::string variant_type_string (VariantType type);

  VariantDef (HirId id, DefId defid, std::string identifier, RustIdent ident,
	      HIR::Expr *discriminant);

  VariantDef (HirId id, DefId defid, std::string identifier, RustIdent ident,
	      VariantType type, HIR::Expr *discriminant,
	      std::vector<StructFieldType *> fields);

  VariantDef (const VariantDef &other);

  VariantDef &operator= (const VariantDef &other);

  static VariantDef &get_error_node ();
  bool is_error () const;

  HirId get_id () const;
  DefId get_defid () const;

  VariantType get_variant_type () const;
  bool is_data_variant () const;
  bool is_dataless_variant () const;

  std::string get_identifier () const;

  size_t num_fields () const;
  StructFieldType *get_field_at_index (size_t index);

  std::vector<StructFieldType *> &get_fields ();

  bool lookup_field (const std::string &lookup, StructFieldType **field_lookup,
		     size_t *index) const;

  HIR::Expr *get_discriminant () const;

  std::string as_string () const;

  bool is_equal (const VariantDef &other) const;

  VariantDef *clone () const;

  VariantDef *monomorphized_clone () const;

  const RustIdent &get_ident () const;

private:
  HirId id;
  DefId defid;
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
  static constexpr auto KIND = TypeKind::ADT;

  enum ADTKind
  {
    STRUCT_STRUCT,
    TUPLE_STRUCT,
    UNION,
    ENUM
  };

  // Representation options, specified via attributes e.g. #[repr(packed)]
  struct ReprOptions
  {
    // bool is_c;
    // bool is_transparent;
    //...

    // For align and pack: 0 = unspecified. Nonzero = byte alignment.
    // It is an error for both to be nonzero, this should be caught when
    // parsing the #[repr] attribute.
    unsigned char align = 0;
    unsigned char pack = 0;
  };

  ADTType (HirId ref, std::string identifier, RustIdent ident, ADTKind adt_kind,
	   std::vector<VariantDef *> variants,
	   std::vector<SubstitutionParamMapping> subst_refs,
	   SubstitutionArgumentMappings generic_arguments
	   = SubstitutionArgumentMappings::error (),
	   RegionConstraints region_constraints = {},
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ADT, ident, refs),
      SubstitutionRef (std::move (subst_refs), std::move (generic_arguments),
		       region_constraints),
      identifier (identifier), variants (variants), adt_kind (adt_kind)
  {}

  ADTType (HirId ref, HirId ty_ref, std::string identifier, RustIdent ident,
	   ADTKind adt_kind, std::vector<VariantDef *> variants,
	   std::vector<SubstitutionParamMapping> subst_refs,
	   SubstitutionArgumentMappings generic_arguments
	   = SubstitutionArgumentMappings::error (),
	   RegionConstraints region_constraints = {},
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ADT, ident, refs),
      SubstitutionRef (std::move (subst_refs), std::move (generic_arguments),
		       region_constraints),
      identifier (identifier), variants (variants), adt_kind (adt_kind)
  {}

  ADTType (HirId ref, HirId ty_ref, std::string identifier, RustIdent ident,
	   ADTKind adt_kind, std::vector<VariantDef *> variants,
	   std::vector<SubstitutionParamMapping> subst_refs, ReprOptions repr,
	   SubstitutionArgumentMappings generic_arguments
	   = SubstitutionArgumentMappings::error (),
	   RegionConstraints region_constraints = {},
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ADT, ident, refs),
      SubstitutionRef (std::move (subst_refs), std::move (generic_arguments),
		       region_constraints),
      identifier (identifier), variants (variants), adt_kind (adt_kind),
      repr (repr)
  {}

  ADTKind get_adt_kind () const { return adt_kind; }

  ReprOptions get_repr_options () const { return repr; }

  bool is_struct_struct () const { return adt_kind == STRUCT_STRUCT; }

  bool is_tuple_struct () const { return adt_kind == TUPLE_STRUCT; }

  bool is_union () const { return adt_kind == UNION; }

  bool is_enum () const { return adt_kind == ENUM; }

  void accept_vis (TyVisitor &vis) override;

  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  std::string get_identifier () const { return identifier; }

  std::string get_name () const override final
  {
    return identifier + subst_as_string ();
  }

  BaseType *clone () const final override;

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
  handle_substitions (SubstitutionArgumentMappings &mappings) override final;

private:
  std::string identifier;
  std::vector<VariantDef *> variants;
  ADTType::ADTKind adt_kind;
  ReprOptions repr;
};

class FnType : public CallableTypeInterface, public SubstitutionRef
{
public:
  static constexpr auto KIND = TypeKind::FNDEF;

  static const uint8_t FNTYPE_DEFAULT_FLAGS = 0x00;
  static const uint8_t FNTYPE_IS_METHOD_FLAG = 0x01;
  static const uint8_t FNTYPE_IS_EXTERN_FLAG = 0x02;
  static const uint8_t FNTYPE_IS_VARADIC_FLAG = 0X04;

  FnType (HirId ref, DefId id, std::string identifier, RustIdent ident,
	  uint8_t flags, ABI abi,
	  std::vector<std::pair<HIR::Pattern *, BaseType *>> params,
	  BaseType *type, std::vector<SubstitutionParamMapping> subst_refs,
	  SubstitutionArgumentMappings substitution_argument_mappings,
	  RegionConstraints region_constraints,
	  std::set<HirId> refs = std::set<HirId> ())
    : CallableTypeInterface (ref, ref, TypeKind::FNDEF, ident, refs),
      SubstitutionRef (std::move (subst_refs), substitution_argument_mappings,
		       region_constraints),
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
	  SubstitutionArgumentMappings substitution_argument_mappings,
	  RegionConstraints region_constraints,
	  std::set<HirId> refs = std::set<HirId> ())
    : CallableTypeInterface (ref, ty_ref, TypeKind::FNDEF, ident, refs),
      SubstitutionRef (std::move (subst_refs), substitution_argument_mappings,
		       region_constraints),
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

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  size_t num_params () const { return params.size (); }

  bool is_method () const
  {
    if (num_params () == 0)
      return false;

    return (flags & FNTYPE_IS_METHOD_FLAG) != 0;
  }

  bool is_extern () const { return (flags & FNTYPE_IS_EXTERN_FLAG) != 0; }

  bool is_variadic () const { return (flags & FNTYPE_IS_VARADIC_FLAG) != 0; }

  DefId get_id () const { return id; }

  // get the Self type for the method
  BaseType *get_self_type () const
  {
    rust_assert (is_method ());
    return param_at (0).second;
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

  BaseType *clone () const final override;

  FnType *
  handle_substitions (SubstitutionArgumentMappings &mappings) override final;

  ABI get_abi () const { return abi; }
  uint8_t get_flags () const { return flags; }

  WARN_UNUSED_RESULT size_t get_num_params () const override
  {
    return params.size ();
  }

  WARN_UNUSED_RESULT BaseType *get_param_type_at (size_t index) const override
  {
    return param_at (index).second;
  }

  WARN_UNUSED_RESULT BaseType *get_return_type () const override
  {
    return type;
  }

private:
  std::vector<std::pair<HIR::Pattern *, BaseType *>> params;
  BaseType *type;
  uint8_t flags;
  std::string identifier;
  DefId id;
  ABI abi;
};

class FnPtr : public CallableTypeInterface
{
public:
  static constexpr auto KIND = TypeKind::FNPTR;

  FnPtr (HirId ref, location_t locus, std::vector<TyVar> params,
	 TyVar result_type, std::set<HirId> refs = std::set<HirId> ())
    : CallableTypeInterface (ref, ref, TypeKind::FNPTR,
			     {Resolver::CanonicalPath::create_empty (), locus},
			     refs),
      params (std::move (params)), result_type (result_type)
  {}

  FnPtr (HirId ref, HirId ty_ref, location_t locus, std::vector<TyVar> params,
	 TyVar result_type, std::set<HirId> refs = std::set<HirId> ())
    : CallableTypeInterface (ref, ty_ref, TypeKind::FNPTR,
			     {Resolver::CanonicalPath::create_empty (), locus},
			     refs),
      params (params), result_type (result_type)
  {}

  std::string get_name () const override final { return as_string (); }

  WARN_UNUSED_RESULT size_t get_num_params () const override
  {
    return params.size ();
  }

  WARN_UNUSED_RESULT BaseType *get_param_type_at (size_t index) const override
  {
    return params.at (index).get_tyty ();
  }

  WARN_UNUSED_RESULT BaseType *get_return_type () const override
  {
    return result_type.get_tyty ();
  }

  const TyVar &get_var_return_type () const { return result_type; }

  size_t num_params () const { return params.size (); }

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;

  std::vector<TyVar> &get_params () { return params; }
  const std::vector<TyVar> &get_params () const { return params; }

private:
  std::vector<TyVar> params;
  TyVar result_type;
};

class ClosureType : public CallableTypeInterface, public SubstitutionRef
{
public:
  static constexpr auto KIND = TypeKind::CLOSURE;

  ClosureType (HirId ref, DefId id, RustIdent ident, TupleType *parameters,
	       TyVar result_type,
	       std::vector<SubstitutionParamMapping> subst_refs,
	       std::set<NodeId> captures,
	       std::set<HirId> refs = std::set<HirId> (),
	       std::vector<TypeBoundPredicate> specified_bounds
	       = std::vector<TypeBoundPredicate> ())
    : CallableTypeInterface (ref, ref, TypeKind::CLOSURE, ident, refs),
      SubstitutionRef (std::move (subst_refs),
		       SubstitutionArgumentMappings::error (),
		       {}), // TODO: check region constraints
      parameters (parameters), result_type (std::move (result_type)), id (id),
      captures (captures)
  {
    LocalDefId local_def_id = id.localDefId;
    rust_assert (local_def_id != UNKNOWN_LOCAL_DEFID);
    inherit_bounds (specified_bounds);
  }

  ClosureType (HirId ref, HirId ty_ref, RustIdent ident, DefId id,
	       TupleType *parameters, TyVar result_type,
	       std::vector<SubstitutionParamMapping> subst_refs,
	       std::set<NodeId> captures,
	       std::set<HirId> refs = std::set<HirId> (),
	       std::vector<TypeBoundPredicate> specified_bounds
	       = std::vector<TypeBoundPredicate> ())
    : CallableTypeInterface (ref, ty_ref, TypeKind::CLOSURE, ident, refs),
      SubstitutionRef (std::move (subst_refs),
		       SubstitutionArgumentMappings::error (), {}), // TODO
      parameters (parameters), result_type (std::move (result_type)), id (id),
      captures (captures)
  {
    LocalDefId local_def_id = id.localDefId;
    rust_assert (local_def_id != UNKNOWN_LOCAL_DEFID);
    inherit_bounds (specified_bounds);
  }

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  WARN_UNUSED_RESULT size_t get_num_params () const override
  {
    return parameters->num_fields ();
  }

  WARN_UNUSED_RESULT BaseType *get_param_type_at (size_t index) const override
  {
    return parameters->get_field (index);
  }

  WARN_UNUSED_RESULT BaseType *get_return_type () const override
  {
    return result_type.get_tyty ();
  }

  std::string as_string () const override;
  std::string get_name () const override final { return as_string (); }

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;

  ClosureType *
  handle_substitions (SubstitutionArgumentMappings &mappings) override final;

  TyTy::TupleType &get_parameters () const { return *parameters; }
  TyTy::BaseType &get_result_type () const { return *result_type.get_tyty (); }

  DefId get_def_id () const { return id; }

  void setup_fn_once_output () const;

  const std::set<NodeId> &get_captures () const { return captures; }

private:
  TyTy::TupleType *parameters;
  TyVar result_type;
  DefId id;
  std::set<NodeId> captures;
};

class ArrayType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::ARRAY;

  ArrayType (HirId ref, location_t locus, HIR::Expr &capacity_expr, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ARRAY,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      element_type (base), capacity_expr (capacity_expr)
  {}

  ArrayType (HirId ref, HirId ty_ref, location_t locus,
	     HIR::Expr &capacity_expr, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ARRAY,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      element_type (base), capacity_expr (capacity_expr)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  BaseType *get_element_type () const;
  const TyVar &get_var_element_type () const;

  BaseType *clone () const final override;

  HIR::Expr &get_capacity_expr () const { return capacity_expr; }

  ArrayType *handle_substitions (SubstitutionArgumentMappings &mappings);

private:
  TyVar element_type;
  // FIXME: I dont think this should be in tyty - tyty should already be const
  // evaluated
  HIR::Expr &capacity_expr;
};

class SliceType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::SLICE;

  SliceType (HirId ref, location_t locus, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::SLICE,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      element_type (base)
  {}

  SliceType (HirId ref, HirId ty_ref, location_t locus, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::SLICE,
		{Resolver::CanonicalPath::create_empty (), locus}, refs),
      element_type (base)
  {}

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  BaseType *get_element_type () const;
  const TyVar &get_var_element_type () const;

  BaseType *clone () const final override;

  SliceType *handle_substitions (SubstitutionArgumentMappings &mappings);

private:
  TyVar element_type;
};

class BoolType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::BOOL;

  BoolType (HirId ref, std::set<HirId> refs = std::set<HirId> ());
  BoolType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

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

  static constexpr auto KIND = TypeKind::INT;

  IntType (HirId ref, IntKind kind, std::set<HirId> refs = std::set<HirId> ());
  IntType (HirId ref, HirId ty_ref, IntKind kind,
	   std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  IntKind get_int_kind () const;

  BaseType *clone () const final override;

  bool is_equal (const BaseType &other) const override;

private:
  IntKind int_kind;
};

class UintType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::UINT;

  enum UintKind
  {
    U8,
    U16,
    U32,
    U64,
    U128
  };

  UintType (HirId ref, UintKind kind,
	    std::set<HirId> refs = std::set<HirId> ());
  UintType (HirId ref, HirId ty_ref, UintKind kind,
	    std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  UintKind get_uint_kind () const;

  BaseType *clone () const final override;

  bool is_equal (const BaseType &other) const override;

private:
  UintKind uint_kind;
};

class FloatType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::FLOAT;

  enum FloatKind
  {
    F32,
    F64
  };

  FloatType (HirId ref, FloatKind kind,
	     std::set<HirId> refs = std::set<HirId> ());
  FloatType (HirId ref, HirId ty_ref, FloatKind kind,
	     std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;
  std::string get_name () const override final;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  FloatKind get_float_kind () const;

  BaseType *clone () const final override;

  bool is_equal (const BaseType &other) const override;

private:
  FloatKind float_kind;
};

class USizeType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::USIZE;

  USizeType (HirId ref, std::set<HirId> refs = std::set<HirId> ());
  USizeType (HirId ref, HirId ty_ref,
	     std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;
  std::string get_name () const override final;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  BaseType *clone () const final override;
};

class ISizeType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::ISIZE;

  ISizeType (HirId ref, std::set<HirId> refs = std::set<HirId> ());
  ISizeType (HirId ref, HirId ty_ref,
	     std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;
  std::string get_name () const override final;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  BaseType *clone () const final override;
};

class CharType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::CHAR;

  CharType (HirId ref, std::set<HirId> refs = std::set<HirId> ());
  CharType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;
  std::string get_name () const override final;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  BaseType *clone () const final override;
};

class StrType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::STR;

  StrType (HirId ref, std::set<HirId> refs = std::set<HirId> ());
  StrType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ());

  std::string get_name () const override final;

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;
};

class DynamicObjectType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::DYNAMIC;

  DynamicObjectType (HirId ref, RustIdent ident,
		     std::vector<TypeBoundPredicate> specified_bounds,
		     std::set<HirId> refs = std::set<HirId> ());

  DynamicObjectType (HirId ref, HirId ty_ref, RustIdent ident,
		     std::vector<TypeBoundPredicate> specified_bounds,
		     std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;

  std::string get_name () const override final;

  // this returns a flat list of items including super trait bounds
  const std::vector<
    std::pair<const Resolver::TraitItemReference *, const TypeBoundPredicate *>>
  get_object_items () const;
};

class ReferenceType : public BaseType
{
public:
  static constexpr auto KIND = REF;

  ReferenceType (HirId ref, TyVar base, Mutability mut,
		 Region region = Region::make_anonymous (),
		 std::set<HirId> refs = std::set<HirId> ());
  ReferenceType (HirId ref, HirId ty_ref, TyVar base, Mutability mut,
		 Region region = Region::make_anonymous (),
		 std::set<HirId> refs = std::set<HirId> ());

  BaseType *get_base () const;
  const TyVar &get_var_element_type () const;

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  std::string get_name () const override final;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;

  ReferenceType *handle_substitions (SubstitutionArgumentMappings &mappings);

  Mutability mutability () const;
  bool is_mutable () const;

  WARN_UNUSED_RESULT Region get_region () const;
  void set_region (Region region);

  bool is_dyn_object () const;
  bool is_dyn_slice_type (const TyTy::SliceType **slice = nullptr) const;
  bool is_dyn_str_type (const TyTy::StrType **str = nullptr) const;
  bool is_dyn_obj_type (const TyTy::DynamicObjectType **dyn = nullptr) const;

private:
  TyVar base;
  Mutability mut;
  Region region;
};

class PointerType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::POINTER;

  PointerType (HirId ref, TyVar base, Mutability mut,
	       std::set<HirId> refs = std::set<HirId> ());
  PointerType (HirId ref, HirId ty_ref, TyVar base, Mutability mut,
	       std::set<HirId> refs = std::set<HirId> ());

  BaseType *get_base () const;
  const TyVar &get_var_element_type () const;

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;
  std::string get_name () const override final;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () const final override;

  PointerType *handle_substitions (SubstitutionArgumentMappings &mappings);

  Mutability mutability () const;
  bool is_mutable () const;
  bool is_const () const;
  bool is_dyn_object () const;
  bool is_dyn_slice_type (const TyTy::SliceType **slice = nullptr) const;
  bool is_dyn_str_type (const TyTy::StrType **str = nullptr) const;
  bool is_dyn_obj_type (const TyTy::DynamicObjectType **dyn = nullptr) const;

private:
  TyVar base;
  Mutability mut;
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
  static constexpr auto KIND = TypeKind::NEVER;

  NeverType (HirId ref, std::set<HirId> refs = std::set<HirId> ());

  NeverType (HirId ref, HirId ty_ref,
	     std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;

  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  BaseType *clone () const final override;

  std::string get_name () const override final;
};

// used at the type in associated types in traits
// see: https://doc.rust-lang.org/book/ch19-03-advanced-traits.html
class PlaceholderType : public BaseType
{
public:
  static constexpr auto KIND = TypeKind::PLACEHOLDER;

  PlaceholderType (std::string symbol, HirId ref,
		   std::set<HirId> refs = std::set<HirId> ());
  PlaceholderType (std::string symbol, HirId ref, HirId ty_ref,
		   std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  BaseType *clone () const final override;

  std::string get_name () const override final;

  std::string get_symbol () const;

  void set_associated_type (HirId ref);

  void clear_associated_type ();

  bool can_resolve () const;

  BaseType *resolve () const;

  bool is_equal (const BaseType &other) const override;

private:
  std::string symbol;
};

class ProjectionType : public BaseType, public SubstitutionRef
{
public:
  static constexpr auto KIND = TypeKind::PROJECTION;

  ProjectionType (HirId ref, BaseType *base,
		  const Resolver::TraitReference *trait, DefId item,
		  std::vector<SubstitutionParamMapping> subst_refs,
		  SubstitutionArgumentMappings generic_arguments
		  = SubstitutionArgumentMappings::error (),
		  RegionConstraints region_constraints = {},
		  std::set<HirId> refs = std::set<HirId> ());

  ProjectionType (HirId ref, HirId ty_ref, BaseType *base,
		  const Resolver::TraitReference *trait, DefId item,
		  std::vector<SubstitutionParamMapping> subst_refs,
		  SubstitutionArgumentMappings generic_arguments
		  = SubstitutionArgumentMappings::error (),
		  RegionConstraints region_constraints = {},
		  std::set<HirId> refs = std::set<HirId> ());

  void accept_vis (TyVisitor &vis) override;
  void accept_vis (TyConstVisitor &vis) const override;

  std::string as_string () const override;

  bool can_eq (const BaseType *other, bool emit_errors) const override final;

  BaseType *clone () const final override;

  std::string get_name () const override final;

  const BaseType *get () const;
  BaseType *get ();

  ProjectionType *
  handle_substitions (SubstitutionArgumentMappings &mappings) override final;

private:
  BaseType *base;
  const Resolver::TraitReference *trait;
  DefId item;
};

template <>
WARN_UNUSED_RESULT inline bool
BaseType::is<CallableTypeInterface> () const
{
  auto kind = this->get_kind ();
  return kind == FNPTR || kind == FNDEF || kind == CLOSURE;
}

template <>
WARN_UNUSED_RESULT inline bool
BaseType::is<const CallableTypeInterface> () const
{
  return this->is<CallableTypeInterface> ();
}

template <>
WARN_UNUSED_RESULT inline bool
BaseType::is<SubstitutionRef> () const
{
  auto kind = this->get_kind ();
  return kind == FNPTR || kind == FNDEF || kind == CLOSURE || kind == ADT
	 || kind == PROJECTION;
}

template <>
WARN_UNUSED_RESULT inline bool
BaseType::is<const SubstitutionRef> () const
{
  return this->is<SubstitutionRef> ();
}

template <>
WARN_UNUSED_RESULT inline SubstitutionRef *
BaseType::as<SubstitutionRef> ()
{
  auto kind = this->get_kind ();
  switch (kind)
    {
    case FNDEF:
      return static_cast<FnType *> (this);
    case CLOSURE:
      return static_cast<ClosureType *> (this);
    case ADT:
      return static_cast<ADTType *> (this);
    case PROJECTION:
      return static_cast<ProjectionType *> (this);
    default:
      rust_unreachable ();
    }
}

template <>
WARN_UNUSED_RESULT inline const SubstitutionRef *
BaseType::as<const SubstitutionRef> () const
{
  auto kind = this->get_kind ();
  switch (kind)
    {
    case FNDEF:
      return static_cast<const FnType *> (this);
    case CLOSURE:
      return static_cast<const ClosureType *> (this);
    case ADT:
      return static_cast<const ADTType *> (this);
    case PROJECTION:
      return static_cast<const ProjectionType *> (this);
    default:
      rust_unreachable ();
    }
}

template <>
WARN_UNUSED_RESULT inline SubstitutionRef *
BaseType::try_as<SubstitutionRef> ()
{
  if (this->is<SubstitutionRef> ())
    {
      return this->as<SubstitutionRef> ();
    }
  return nullptr;
}

template <>
WARN_UNUSED_RESULT inline const SubstitutionRef *
BaseType::try_as<const SubstitutionRef> () const
{
  if (this->is<const SubstitutionRef> ())
    {
      return this->as<const SubstitutionRef> ();
    }
  return nullptr;
}

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY
