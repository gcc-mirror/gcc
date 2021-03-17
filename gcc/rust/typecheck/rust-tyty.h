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

#include "rust-hir-map.h"

namespace Rust {
namespace TyTy {

// https://rustc-dev-guide.rust-lang.org/type-inference.html#inference-variables
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/enum.TyKind.html#variants
enum TypeKind
{
  INFER,
  ADT,
  STR,
  REF,
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
  UNIT,
  USIZE,
  ISIZE,
  // there are more to add...
  ERROR
};

class TyVisitor;
class BaseType
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

  /* Visitor pattern for double dispatch. BaseRules implements TyVisitor. */
  virtual void accept_vis (TyVisitor &vis) = 0;

  virtual std::string as_string () const = 0;

  virtual std::string get_name () const = 0;

  /* Unify two types. Returns a pointer to the newly-created unified ty, or
     nullptr if the two ty cannot be unified. The caller is responsible for
     releasing the memory of the returned ty. */
  virtual BaseType *unify (BaseType *other) = 0;

  /* Check value equality between two ty. Type inference rules are ignored. Two
     ty are considered equal if they're of the same kind, and
       1. (For ADTs, arrays, tuples, refs) have the same underlying ty
       2. (For functions) have the same signature */
  virtual bool is_equal (const BaseType &other) const
  {
    return get_kind () == other.get_kind ();
  }

  virtual bool is_unit () const { return kind == TypeKind::UNIT; }

  TypeKind get_kind () const { return kind; }

  /* Returns a pointer to a clone of this. The caller is responsible for
   * releasing the memory of the returned ty. */
  virtual BaseType *clone () = 0;

  // get_combined_refs returns the chain of node refs involved in unification
  std::set<HirId> get_combined_refs () { return combined; }

  void append_reference (HirId id) { combined.insert (id); }

  virtual bool supports_substitutions () const { return false; }

  virtual bool has_subsititions_defined () const { return false; }

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
    return as_string () + ":" + mappings_str ();
  }

  void debug () const { printf ("%s\n", debug_str ().c_str ()); }

protected:
  BaseType (HirId ref, HirId ty_ref, TypeKind kind,
	    std::set<HirId> refs = std::set<HirId> ())
    : kind (kind), ref (ref), ty_ref (ty_ref), combined (refs),
      mappings (Analysis::Mappings::get ())
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

  bool is_unit () const override { return false; }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  BaseType *clone () final override;

  InferTypeKind get_infer_kind () const { return infer_kind; }

  std::string get_name () const override final { return as_string (); }

  bool default_type (BaseType **type) const;

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

  bool is_unit () const override { return true; }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  BaseType *clone () final override;

  std::string get_name () const override final { return as_string (); }
};

class UnitType : public BaseType
{
public:
  UnitType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::UNIT, refs)
  {}

  UnitType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::UNIT, refs)
  {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return true; }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  BaseType *clone () final override;

  std::string get_name () const override final { return as_string (); }
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

  StructFieldType *clone () const;

private:
  HirId ref;
  std::string name;
  BaseType *ty;
};

class TupleType : public BaseType
{
public:
  TupleType (HirId ref, std::vector<TyVar> fields,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::TUPLE, refs), fields (fields)
  {}

  TupleType (HirId ref, HirId ty_ref, std::vector<TyVar> fields,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::TUPLE, refs), fields (fields)
  {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return false; }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  size_t num_fields () const { return fields.size (); }

  BaseType *get_field (size_t index) const;

  BaseType *clone () final override;

  void iterate_fields (std::function<bool (BaseType *)> cb) const
  {
    for (size_t i = 0; i < num_fields (); i++)
      {
	if (!cb (get_field (i)))
	  return;
      }
  }

  std::string get_name () const override final { return as_string (); }

private:
  std::vector<TyVar> fields;
};

class ParamType : public BaseType
{
public:
  ParamType (std::string symbol, HirId ref, HIR::GenericParam &param,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::PARAM), symbol (symbol), param (param)
  {}

  ParamType (std::string symbol, HirId ref, HirId ty_ref,
	     HIR::GenericParam &param,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::PARAM), symbol (symbol), param (param)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  BaseType *clone () final override;

  std::string get_symbol () const;

  HIR::GenericParam &get_generic_param () { return param; }

  bool can_resolve () const { return get_ref () != get_ty_ref (); }

  BaseType *resolve ();

  std::string get_name () const override final { return as_string (); }

private:
  std::string symbol;
  HIR::GenericParam &param;
};

class SubstitutionMapping
{
public:
  SubstitutionMapping (std::unique_ptr<HIR::GenericParam> &generic,
		       ParamType *param)
    : generic (generic), param (param)
  {}

  std::string as_string () const { return param->as_string (); }

  void fill_param_ty (BaseType *type) { param->set_ty_ref (type->get_ref ()); }

  SubstitutionMapping clone ()
  {
    return SubstitutionMapping (generic,
				static_cast<ParamType *> (param->clone ()));
  }

  const ParamType *get_param_ty () const { return param; }

private:
  std::unique_ptr<HIR::GenericParam> &generic;
  ParamType *param;
};

template <class T> class SubstitutionRef
{
public:
  SubstitutionRef (std::vector<SubstitutionMapping> substitutions)
    : substitutions (substitutions)
  {}

  bool has_substitutions () const { return substitutions.size () > 0; }

  std::string subst_as_string () const
  {
    std::string buffer;
    for (size_t i = 0; i < substitutions.size (); i++)
      {
	const SubstitutionMapping &sub = substitutions.at (i);
	buffer += sub.as_string ();

	if ((i + 1) < substitutions.size ())
	  buffer += ", ";
      }

    return buffer.empty () ? "" : "<" + buffer + ">";
  }

  size_t get_num_substitutions () const { return substitutions.size (); }

  std::vector<SubstitutionMapping> &get_substs () { return substitutions; }

  std::vector<SubstitutionMapping> clone_substs ()
  {
    std::vector<SubstitutionMapping> clone;
    for (auto &sub : substitutions)
      clone.push_back (sub.clone ());

    return clone;
  }

  virtual T *infer_substitutions () = 0;

  virtual T *handle_substitutions (HIR::GenericArgs &generic_args) = 0;

protected:
  virtual void fill_in_at (size_t index, BaseType *type)
  {
    substitutions.at (index).fill_param_ty (type);
  }

  SubstitutionMapping get_substitution_mapping_at (size_t index)
  {
    return substitutions.at (index);
  }

private:
  std::vector<SubstitutionMapping> substitutions;
};

class ADTType : public BaseType, public SubstitutionRef<ADTType>
{
public:
  ADTType (HirId ref, std::string identifier, bool is_tuple,
	   std::vector<StructFieldType *> fields,
	   std::vector<SubstitutionMapping> subst_refs,
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ADT, refs),
      SubstitutionRef (std::move (subst_refs)), identifier (identifier),
      fields (fields), is_tuple (is_tuple)
  {}

  ADTType (HirId ref, HirId ty_ref, std::string identifier, bool is_tuple,
	   std::vector<StructFieldType *> fields,
	   std::vector<SubstitutionMapping> subst_refs,
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ADT, refs),
      SubstitutionRef (std::move (subst_refs)), identifier (identifier),
      fields (fields), is_tuple (is_tuple)
  {}

  bool get_is_tuple () { return is_tuple; }

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return false; }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  size_t num_fields () const { return fields.size (); }

  std::string get_name () const override final
  {
    return identifier + subst_as_string ();
  }

  BaseType *get_field_type (size_t index);

  const BaseType *get_field_type (size_t index) const;

  const StructFieldType *get_field (size_t index) const;

  StructFieldType *get_field (size_t index) { return fields.at (index); }

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

  BaseType *clone () final override;

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

  bool supports_substitutions () const override final { return true; }

  bool has_subsititions_defined () const override final
  {
    return has_substitutions ();
  }

  ADTType *infer_substitutions () override final;

  ADTType *handle_substitutions (HIR::GenericArgs &generic_args) override final;

  void fill_in_at (size_t index, BaseType *type) override final;

  void fill_in_params_for (SubstitutionMapping sub, BaseType *type);

private:
  std::string identifier;
  std::vector<StructFieldType *> fields;
  bool is_tuple;
};

class FnType : public BaseType
{
public:
  FnType (HirId ref, std::vector<std::pair<HIR::Pattern *, BaseType *> > params,
	  BaseType *type, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::FNDEF, refs), params (std::move (params)),
      type (type)
  {}

  FnType (HirId ref, HirId ty_ref,
	  std::vector<std::pair<HIR::Pattern *, BaseType *> > params,
	  BaseType *type, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::FNDEF, refs), params (params),
      type (type)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  size_t num_params () const { return params.size (); }

  std::vector<std::pair<HIR::Pattern *, BaseType *> > &get_params ()
  {
    return params;
  }

  const std::vector<std::pair<HIR::Pattern *, BaseType *> > &get_params () const
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

  BaseType *clone () final override;

private:
  std::vector<std::pair<HIR::Pattern *, BaseType *> > params;
  BaseType *type;
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

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () final override;

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
  ArrayType (HirId ref, size_t capacity, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ARRAY, refs), capacity (capacity),
      element_type (base)
  {}

  ArrayType (HirId ref, HirId ty_ref, size_t capacity, TyVar base,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ARRAY, refs), capacity (capacity),
      element_type (base)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  size_t get_capacity () const { return capacity; }

  BaseType *get_element_type () const;

  BaseType *clone () final override;

private:
  size_t capacity;
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

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;

  BaseType *clone () final override;
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

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;

  IntKind get_kind () const { return int_kind; }

  BaseType *clone () final override;

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

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;

  UintKind get_kind () const { return uint_kind; }

  BaseType *clone () final override;

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

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;

  FloatKind get_kind () const { return float_kind; }

  BaseType *clone () final override;

private:
  FloatKind float_kind;
};

class USizeType : public BaseType
{
public:
  USizeType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::USIZE)
  {}

  USizeType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::USIZE)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;

  BaseType *clone () final override;
};

class ISizeType : public BaseType
{
public:
  ISizeType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ISIZE)
  {}

  ISizeType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ISIZE)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;

  BaseType *clone () final override;
};

class CharType : public BaseType
{
public:
  CharType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::CHAR)
  {}

  CharType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::CHAR)

  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;

  BaseType *clone () final override;
};

class ReferenceType : public BaseType
{
public:
  ReferenceType (HirId ref, TyVar base,
		 std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::REF), base (base)
  {}

  ReferenceType (HirId ref, HirId ty_ref, TyVar base,
		 std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::REF), base (base)
  {}

  BaseType *get_base () const;

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  std::string get_name () const override final { return as_string (); }

  BaseType *unify (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () final override;

private:
  TyVar base;
};

class StrType : public BaseType
{
public:
  StrType (HirId ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::STR)
  {}

  StrType (HirId ref, HirId ty_ref, std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::STR)
  {}

  std::string get_name () const override final { return as_string (); }

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  bool is_equal (const BaseType &other) const override;

  BaseType *clone () final override;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY
