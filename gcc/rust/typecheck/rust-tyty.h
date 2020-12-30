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
};

class TyVisitor;
class TyBase
{
public:
  ~TyBase () {}

  HirId get_ref () const { return ref; }

  virtual void accept_vis (TyVisitor &vis) = 0;

  virtual std::string as_string () const = 0;

  virtual TyBase *combine (TyBase *other) = 0;

  virtual bool is_unit () const { return kind == TypeKind::UNIT; }

  TypeKind get_kind () const { return kind; }

protected:
  TyBase (HirId ref, TypeKind kind) : kind (kind), ref (ref) {}

  TypeKind kind;
  HirId ref;
};

class InferType : public TyBase
{
public:
  InferType (HirId ref) : TyBase (ref, TypeKind::INFER) {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return true; }

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;
};

class UnitType : public TyBase
{
public:
  UnitType (HirId ref) : TyBase (ref, TypeKind::UNIT) {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return true; }

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;
};

class StructFieldType : public TyBase
{
public:
  StructFieldType (HirId ref, std::string name, TyBase *ty)
    : TyBase (ref, TypeKind::FIELD), name (name), ty (ty)
  {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return ty->is_unit (); }

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  std::string get_name () const { return name; }

  TyBase *get_field_type () { return ty; }

private:
  std::string name;
  TyBase *ty;
};

class ADTType : public TyBase
{
public:
  ADTType (HirId ref, std::string identifier,
	   std::vector<StructFieldType *> fields)
    : TyBase (ref, TypeKind::ADT), identifier (identifier), fields (fields)
  {}

  void accept_vis (TyVisitor &vis) override;

  bool is_unit () const override { return false; }

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  size_t num_fields () const { return fields.size (); }

  StructFieldType *get_field (size_t index) { return fields.at (index); }

  StructFieldType *get_field (const std::string &lookup)
  {
    for (auto &field : fields)
      {
	if (field->get_name ().compare (lookup) == 0)
	  return field;
      }
    return nullptr;
  }

private:
  std::string identifier;
  std::vector<StructFieldType *> fields;
};

class ParamType : public TyBase
{
public:
  ParamType (HirId ref, std::string identifier, TyBase *type)
    : TyBase (ref, TypeKind::PARAM), identifier (identifier), type (type)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  std::string get_identifier () const { return identifier; }

  TyBase *get_base_type () { return type; }

private:
  std::string identifier;
  TyBase *type;
};

class FnType : public TyBase
{
public:
  FnType (HirId ref, std::vector<ParamType *> params, TyBase *type)
    : TyBase (ref, TypeKind::FNDEF), params (params), type (type)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *return_type () { return type; }

  TyBase *combine (TyBase *other) override;

  size_t num_params () const { return params.size (); }

  ParamType *param_at (size_t idx) { return params[idx]; }

  TyBase *get_return_type () { return type; }

private:
  std::vector<ParamType *> params;
  TyBase *type;
};

class ArrayType : public TyBase
{
public:
  ArrayType (HirId ref, size_t capacity, TyBase *type)
    : TyBase (ref, TypeKind::ARRAY), capacity (capacity), type (type)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  size_t get_capacity () const { return capacity; }

  TyBase *get_type () { return type; }

private:
  size_t capacity;
  TyBase *type;
};

class BoolType : public TyBase
{
public:
  BoolType (HirId ref) : TyBase (ref, TypeKind::BOOL) {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;
};

class IntType : public TyBase
{
public:
  enum IntKind
  {
    I8,
    I16,
    I32,
  };

  IntType (HirId ref, IntKind kind)
    : TyBase (ref, TypeKind::INT), int_kind (kind)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  IntKind get_kind () const { return int_kind; }

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
  };

  UintType (HirId ref, UintKind kind)
    : TyBase (ref, TypeKind::UINT), uint_kind (kind)
  {}

  void accept_vis (TyVisitor &vis) override;

  std::string as_string () const override;

  TyBase *combine (TyBase *other) override;

  UintKind get_kind () const { return uint_kind; }

private:
  UintKind uint_kind;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY
