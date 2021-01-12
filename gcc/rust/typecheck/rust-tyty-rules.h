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

#ifndef RUST_TYTY_RULES
#define RUST_TYTY_RULES

#include "rust-diagnostics.h"
#include "rust-tyty.h"
#include "rust-tyty-visitor.h"
#include "rust-hir-map.h"

namespace Rust {
namespace TyTy {

class BaseRules : public TyVisitor
{
public:
  virtual ~BaseRules () {}

  virtual void visit (UnitType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (ADTType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (InferType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (FnType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (ParamType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (ArrayType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (BoolType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (IntType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (UintType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (FloatType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (ErrorType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

  virtual void visit (StructFieldType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location def_locus = mappings->lookup_location (base->get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
    rust_error_at (def_locus, "declared here");
  }

protected:
  BaseRules (TyBase *base)
    : mappings (Analysis::Mappings::get ()), base (base),
      resolved (new ErrorType (base->get_ref (), base->get_ref ()))
  {}

  Analysis::Mappings *mappings;
  TyBase *base;
  TyBase *resolved;
};

class InferRules : protected BaseRules
{
public:
  InferRules (InferType *base) : BaseRules (base), base (base) {}

  TyBase *combine (TyBase *other)
  {
    other->accept_vis (*this);
    return resolved;
  }

  // we are an inference variable so this means we can take the other as the
  // type
  void visit (InferType &type) override { resolved = type.clone (); }

  void visit (UnitType &type) override { resolved = type.clone (); }

  void visit (BoolType &type) override { resolved = type.clone (); }

  void visit (IntType &type) override { resolved = type.clone (); }

  void visit (UintType &type) override { resolved = type.clone (); }

  void visit (FloatType &type) override { resolved = type.clone (); }

  void visit (ParamType &type) override { resolved = type.clone (); }

  void visit (ArrayType &type) override { resolved = type.clone (); }

  void visit (ADTType &type) override { resolved = type.clone (); }

private:
  InferType *base;
};

class StructFieldTypeRules : protected BaseRules
{
public:
  StructFieldTypeRules (StructFieldType *base) : BaseRules (base), base (base)
  {}

  TyBase *combine (TyBase *other)
  {
    other->accept_vis (*this);
    return resolved;
  }

private:
  StructFieldType *base;
};

class UnitRules : protected BaseRules
{
public:
  UnitRules (UnitType *base) : BaseRules (base), base (base) {}

  TyBase *combine (TyBase *other)
  {
    other->accept_vis (*this);
    return resolved;
  }

  void visit (UnitType &type) override
  {
    resolved = new UnitType (type.get_ref (), type.get_ty_ref ());
  }

private:
  UnitType *base;
};

class FnRules : protected BaseRules
{
public:
  FnRules (FnType *base) : BaseRules (base), base (base) {}

  TyBase *combine (TyBase *other)
  {
    other->accept_vis (*this);
    return resolved;
  }

private:
  FnType *base;
};

class ParamRules : protected BaseRules
{
public:
  ParamRules (ParamType *base) : BaseRules (base), base (base) {}

  TyBase *combine (TyBase *other)
  {
    // we only case about the base type of a param
    return base->get_base_type ()->combine (other);
  }

private:
  ParamType *base;
};

class ArrayRules : protected BaseRules
{
public:
  ArrayRules (ArrayType *base) : BaseRules (base), base (base) {}

  TyBase *combine (TyBase *other)
  {
    other->accept_vis (*this);
    return resolved;
  }

  void visit (ArrayType &type) override
  {
    // check base type
    auto base_resolved = base->get_type ()->combine (type.get_type ());
    if (base_resolved == nullptr)
      return;

    // need to check the base types and capacity
    if (type.get_capacity () != base->get_capacity ())
      {
	Location locus = mappings->lookup_location (type.get_ref ());
	rust_error_at (locus, "mismatch in array capacity");
	return;
      }

    resolved = new ArrayType (type.get_ref (), type.get_ty_ref (),
			      type.get_capacity (), base_resolved);
  }

private:
  ArrayType *base;
};

class BoolRules : protected BaseRules
{
public:
  BoolRules (BoolType *base) : BaseRules (base), base (base) {}

  TyBase *combine (TyBase *other)
  {
    other->accept_vis (*this);
    return resolved;
  }

  void visit (BoolType &type) override
  {
    resolved = new BoolType (type.get_ref (), type.get_ty_ref ());
  }

private:
  BoolType *base;
};

class IntRules : protected BaseRules
{
public:
  IntRules (IntType *base) : BaseRules (base), base (base) {}

  TyBase *combine (TyBase *other)
  {
    other->accept_vis (*this);
    return resolved;
  }

  void visit (IntType &type) override
  {
    if (type.get_kind () != base->get_kind ())
      {
	BaseRules::visit (type);
	return;
      }

    resolved
      = new IntType (type.get_ref (), type.get_ty_ref (), type.get_kind ());
  }

private:
  IntType *base;
};

class UintRules : protected BaseRules
{
public:
  UintRules (UintType *base) : BaseRules (base), base (base) {}

  TyBase *combine (TyBase *other)
  {
    other->accept_vis (*this);
    return resolved;
  }

  void visit (UintType &type) override
  {
    if (type.get_kind () != base->get_kind ())
      {
	BaseRules::visit (type);
	return;
      }

    resolved
      = new UintType (type.get_ref (), type.get_ty_ref (), type.get_kind ());
  }

private:
  UintType *base;
};

class FloatRules : protected BaseRules
{
public:
  FloatRules (FloatType *base) : BaseRules (base), base (base) {}

  TyBase *combine (TyBase *other)
  {
    other->accept_vis (*this);
    return resolved;
  }

  void visit (FloatType &type) override
  {
    if (type.get_kind () != base->get_kind ())
      {
	BaseRules::visit (type);
	return;
      }

    resolved
      = new FloatType (type.get_ref (), type.get_ty_ref (), type.get_kind ());
  }

private:
  FloatType *base;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_RULES
