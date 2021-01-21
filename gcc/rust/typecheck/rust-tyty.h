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
  // there are more to add...
  ERROR
};

class TyVisitor;
class TyBase
{
public:
  virtual ~TyBase () {}

  HirId get_ref () const { return ref; }

  void set_ref (HirId id) { ref = id; }

  HirId get_ty_ref () const { return ty_ref; }

  void set_ty_ref (HirId id) { ty_ref = id; }

  virtual void accept_vis (TyVisitor &vis) = 0;

  virtual std::string as_string () const = 0;

  virtual TyBase *combine (TyBase *other) = 0;

  virtual bool is_unit () const { return kind == TypeKind::UNIT; }

  TypeKind get_kind () const { return kind; }

  virtual TyBase *clone () = 0;

protected:
  TyBase (HirId ref, HirId ty_ref, TypeKind kind)
    : kind (kind), ref (ref), ty_ref (ty_ref)
  {}

  TypeKind kind;
  HirId ref;
  HirId ty_ref;
};

class InferType : public TyBase
{
public:
  InferType (HirId ref) : TyBase (ref, ref, TypeKind::INFER) {}

  InferType (HirId ref, HirId ty_ref) : TyBase (ref, ty_ref, TypeKind::INFER) {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return true; }

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  TyBase *clone () final override;
};

class ErrorType : public TyBase
{
public:
  ErrorType (HirId ref) : TyBase (ref, ref, TypeKind::ERROR) {}

  ErrorType (HirId ref, HirId ty_ref) : TyBase (ref, ty_ref, TypeKind::ERROR) {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return true; }

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  TyBase *clone () final override;
};

class UnitType : public TyBase
{
public:
  UnitType (HirId ref) : TyBase (ref, ref, TypeKind::UNIT) {}

  UnitType (HirId ref, HirId ty_ref) : TyBase (ref, ty_ref, TypeKind::UNIT) {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return true; }

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  TyBase *clone () final override;
};

class StructFieldType : public TyBase
{
public:
  StructFieldType (HirId ref, std::string name, TyBase *ty)
    : TyBase (ref, ref, TypeKind::FIELD), name (name), ty (ty)
  {}

  StructFieldType (HirId ref, HirId ty_ref, std::string name, TyBase *ty)
    : TyBase (ref, ty_ref, TypeKind::FIELD), name (name), ty (ty)
  {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return ty->is_unit (); }

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  std::string get_name () const { return name; }

  TyBase *get_field_type () { return ty; }

  TyBase *clone () final override;

private:
  std::string name;
  TyBase *ty;
};

class ADTType : public TyBase
{
public:
  ADTType (HirId ref, std::string identifier, bool is_tuple,
	   std::vector<StructFieldType *> fields)
    : TyBase (ref, ref, TypeKind::ADT), identifier (identifier),
      is_tuple (is_tuple), fields (fields)
  {}

  ADTType (HirId ref, HirId ty_ref, std::string identifier, bool is_tuple,
	   std::vector<StructFieldType *> fields)
    : TyBase (ref, ty_ref, TypeKind::ADT), identifier (identifier),
      is_tuple (is_tuple), fields (fields)
  {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return false; }

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  size_t num_fields () const { return fields.size (); }

  std::string get_name () const { return identifier; }

  bool is_tuple_struct () const { return is_tuple; }

  StructFieldType *get_field (size_t index) { return fields.at (index); }

  StructFieldType *get_field (const std::string &lookup,
			      size_t *index = nullptr)
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

  TyBase *clone () final override;

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
  bool is_tuple;
  std::vector<StructFieldType *> fields;
};

class FnType : public TyBase
{
public:
  FnType (HirId ref, std::vector<std::pair<HIR::Pattern *, TyBase *> > params,
	  TyBase *type)
    : TyBase (ref, ref, TypeKind::FNDEF), params (std::move (params)),
      type (type)
  {}

  FnType (HirId ref, HirId ty_ref,
	  std::vector<std::pair<HIR::Pattern *, TyBase *> > params,
	  TyBase *type)
    : TyBase (ref, ty_ref, TypeKind::FNDEF), params (params), type (type)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *return_type () { return type; }

  TyBase *combine (TyBase *other) override;

  size_t num_params () const { return params.size (); }

  std::vector<std::pair<HIR::Pattern *, TyBase *> > &get_params ()
  {
    return params;
  }

  std::pair<HIR::Pattern *, TyBase *> &param_at (size_t idx)
  {
    return params[idx];
  }

  TyBase *get_return_type () { return type; }

  TyBase *clone () final override;

private:
  std::vector<std::pair<HIR::Pattern *, TyBase *> > params;
  TyBase *type;
};

class ArrayType : public TyBase
{
public:
  ArrayType (HirId ref, size_t capacity, TyBase *type)
    : TyBase (ref, ref, TypeKind::ARRAY), capacity (capacity), type (type)
  {}

  ArrayType (HirId ref, HirId ty_ref, size_t capacity, TyBase *type)
    : TyBase (ref, ty_ref, TypeKind::ARRAY), capacity (capacity), type (type)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  size_t get_capacity () const { return capacity; }

  TyBase *get_type () { return type; }

  TyBase *clone () final override;

private:
  size_t capacity;
  TyBase *type;
};

class BoolType : public TyBase
{
public:
  BoolType (HirId ref) : TyBase (ref, ref, TypeKind::BOOL) {}

  BoolType (HirId ref, HirId ty_ref) : TyBase (ref, ty_ref, TypeKind::BOOL) {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  TyBase *clone () final override;
};

class IntType : public TyBase
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

  IntType (HirId ref, IntKind kind)
    : TyBase (ref, ref, TypeKind::INT), int_kind (kind)
  {}

  IntType (HirId ref, HirId ty_ref, IntKind kind)
    : TyBase (ref, ty_ref, TypeKind::INT), int_kind (kind)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  IntKind get_kind () const { return int_kind; }

  TyBase *clone () final override;

private:
  IntKind int_kind;
};

class UintType : public TyBase
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

  UintType (HirId ref, UintKind kind)
    : TyBase (ref, ref, TypeKind::UINT), uint_kind (kind)
  {}

  UintType (HirId ref, HirId ty_ref, UintKind kind)
    : TyBase (ref, ty_ref, TypeKind::UINT), uint_kind (kind)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  UintKind get_kind () const { return uint_kind; }

  TyBase *clone () final override;

private:
  UintKind uint_kind;
};

class FloatType : public TyBase
{
public:
  enum FloatKind
  {
    F32,
    F64
  };

  FloatType (HirId ref, FloatKind kind)
    : TyBase (ref, ref, TypeKind::FLOAT), float_kind (kind)
  {}

  FloatType (HirId ref, HirId ty_ref, FloatKind kind)
    : TyBase (ref, ty_ref, TypeKind::FLOAT), float_kind (kind)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  FloatKind get_kind () const { return float_kind; }

  TyBase *clone () final override;

private:
  FloatKind float_kind;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY
