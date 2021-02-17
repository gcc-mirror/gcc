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
  TUPLE,
  BOOL,
  CHAR,
  INT,
  UINT,
  FLOAT,
  UNIT,
  FIELD,
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

  void set_ref (HirId id) { ref = id; }

  HirId get_ty_ref () const { return ty_ref; }

  void set_ty_ref (HirId id) { ty_ref = id; }

  /* Visitor pattern for double dispatch. BaseRules implements TyVisitor. */
  virtual void accept_vis (TyVisitor &vis) = 0;

  virtual std::string as_string () const = 0;

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

  std::set<HirId> get_combined_refs () { return combined; }

  void append_reference (HirId id) { combined.insert (id); }

protected:
  BaseType (HirId ref, HirId ty_ref, TypeKind kind,
	    std::set<HirId> refs = std::set<HirId> ())
    : kind (kind), ref (ref), ty_ref (ty_ref), combined (refs)
  {}

  TypeKind kind;
  HirId ref;
  HirId ty_ref;

  std::set<HirId> combined;
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

  bool is_unit () const override { return true; }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  BaseType *clone () final override;

  InferTypeKind get_infer_kind () const { return infer_kind; }

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
};

class StructFieldType : public BaseType
{
public:
  StructFieldType (HirId ref, std::string name, BaseType *ty,
		   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::FIELD, refs), name (name), ty (ty)
  {}

  StructFieldType (HirId ref, HirId ty_ref, std::string name, BaseType *ty,
		   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::FIELD, refs), name (name), ty (ty)
  {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return ty->is_unit (); }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  virtual bool is_equal (const BaseType &other) const override;

  std::string get_name () const { return name; }

  BaseType *get_field_type () const { return ty; }

  BaseType *clone () final override;

private:
  std::string name;
  BaseType *ty;
};

class TupleType : public BaseType
{
public:
  TupleType (HirId ref, std::vector<HirId> fields,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::TUPLE, refs), fields (fields)
  {}

  TupleType (HirId ref, HirId ty_ref, std::vector<HirId> fields,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::TUPLE, refs), fields (fields)
  {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return false; }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  virtual bool is_equal (const BaseType &other) const override;

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

private:
  std::vector<HirId> fields;
};

class ADTType : public BaseType
{
public:
  ADTType (HirId ref, std::string identifier,
	   std::vector<StructFieldType *> fields,
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ADT, refs), identifier (identifier),
      fields (fields)
  {}

  ADTType (HirId ref, HirId ty_ref, std::string identifier,
	   std::vector<StructFieldType *> fields,
	   std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ADT, refs), identifier (identifier),
      fields (fields)
  {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return false; }

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  virtual bool is_equal (const BaseType &other) const override;

  size_t num_fields () const { return fields.size (); }

  std::string get_name () const { return identifier; }

  StructFieldType *get_field (size_t index) const { return fields.at (index); }

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

private:
  std::string identifier;
  std::vector<StructFieldType *> fields;
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

  BaseType *return_type () { return type; }

  BaseType *unify (BaseType *other) override;

  virtual bool is_equal (const BaseType &other) const override;

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

class ArrayType : public BaseType
{
public:
  ArrayType (HirId ref, size_t capacity, BaseType *type,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::ARRAY, refs), capacity (capacity),
      element_type_id (type->get_ref ())
  {}

  ArrayType (HirId ref, HirId ty_ref, size_t capacity, BaseType *type,
	     std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::ARRAY, refs), capacity (capacity),
      element_type_id (type->get_ref ())
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  virtual bool is_equal (const BaseType &other) const override;

  size_t get_capacity () const { return capacity; }

  HirId element_type_ref () const { return element_type_id; }

  BaseType *get_type () const;

  BaseType *clone () final override;

private:
  size_t capacity;
  HirId element_type_id;
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

  BaseType *unify (BaseType *other) override;

  BaseType *clone () final override;
};

class ReferenceType : public BaseType
{
public:
  ReferenceType (HirId ref, HirId base,
		 std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ref, TypeKind::REF), base (base)
  {}

  ReferenceType (HirId ref, HirId ty_ref, HirId base,
		 std::set<HirId> refs = std::set<HirId> ())
    : BaseType (ref, ty_ref, TypeKind::REF), base (base)
  {}

  const TyTy::BaseType *get_base () const;

  TyTy::BaseType *get_base ();

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  BaseType *unify (BaseType *other) override;

  virtual bool is_equal (const BaseType &other) const override;

  BaseType *clone () final override;

private:
  HirId base;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY
