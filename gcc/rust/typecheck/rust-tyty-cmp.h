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

#ifndef RUST_TYTY_CMP_H
#define RUST_TYTY_CMP_H

#include "rust-diagnostics.h"
#include "rust-tyty.h"
#include "rust-tyty-visitor.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace TyTy {

class BaseCmp : public TyVisitor
{
public:
  virtual bool can_eq (BaseType *other)
  {
    if (other->get_kind () == TypeKind::PARAM)
      {
	ParamType *p = static_cast<ParamType *> (other);
	if (p->can_resolve ())
	  {
	    other = p->resolve ();
	  }
      }

    other->accept_vis (*this);
    return ok;
  }

  virtual void visit (TupleType &) override { ok = false; }

  virtual void visit (ADTType &) override { ok = false; }

  virtual void visit (InferType &) override { ok = false; }

  virtual void visit (FnType &) override { ok = false; }

  virtual void visit (FnPtr &) override { ok = false; }

  virtual void visit (ArrayType &) override { ok = false; }

  virtual void visit (BoolType &) override { ok = false; }

  virtual void visit (IntType &) override { ok = false; }

  virtual void visit (UintType &) override { ok = false; }

  virtual void visit (USizeType &) override { ok = false; }

  virtual void visit (ISizeType &) override { ok = false; }

  virtual void visit (FloatType &) override { ok = false; }

  virtual void visit (ErrorType &) override { ok = false; }

  virtual void visit (CharType &) override { ok = false; }

  virtual void visit (ReferenceType &) override { ok = false; }

  virtual void visit (ParamType &) override
  {
    // it is ok for types to can eq to a ParamType
    ok = true;
  }

  virtual void visit (StrType &) override { ok = false; }

  virtual void visit (NeverType &) override { ok = false; }

  virtual void visit (PlaceholderType &) override
  { // it is ok for types to can eq to a placeholder
    ok = true;
  }

protected:
  BaseCmp (BaseType *base)
    : mappings (Analysis::Mappings::get ()),
      context (Resolver::TypeCheckContext::get ()), ok (false)
  {}

  Analysis::Mappings *mappings;
  Resolver::TypeCheckContext *context;

  bool ok;

private:
  /* Returns a pointer to the ty that created this rule. */
  virtual BaseType *get_base () = 0;
};

class InferCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  InferCmp (InferType *base) : BaseCmp (base), base (base) {}

  void visit (BoolType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (IntType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (UintType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (USizeType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (ISizeType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (FloatType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind () == TyTy::InferType::InferTypeKind::FLOAT);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (ArrayType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (ADTType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (TupleType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (InferType &type) override
  {
    switch (base->get_infer_kind ())
      {
      case InferType::InferTypeKind::GENERAL:
	ok = true;
	return;

	case InferType::InferTypeKind::INTEGRAL: {
	  if (type.get_infer_kind () == InferType::InferTypeKind::INTEGRAL)
	    {
	      ok = true;
	      return;
	    }
	  else if (type.get_infer_kind () == InferType::InferTypeKind::GENERAL)
	    {
	      ok = true;
	      return;
	    }
	}
	break;

	case InferType::InferTypeKind::FLOAT: {
	  if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
	    {
	      ok = true;
	      return;
	    }
	  else if (type.get_infer_kind () == InferType::InferTypeKind::GENERAL)
	    {
	      ok = true;
	      return;
	    }
	}
	break;
      }

    BaseCmp::visit (type);
  }

  void visit (CharType &type) override
  {
    {
      bool is_valid
	= (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
      if (is_valid)
	{
	  ok = true;
	  return;
	}

      BaseCmp::visit (type);
    }
  }

  void visit (ReferenceType &type) override

  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (ParamType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

private:
  BaseType *get_base () override { return base; }

  InferType *base;
};

class FnCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  FnCmp (FnType *base) : BaseCmp (base), base (base) {}

  void visit (InferType &type) override
  {
    ok = type.get_infer_kind () == InferType::InferTypeKind::GENERAL;
  }

  void visit (FnType &type) override
  {
    if (base->num_params () != type.num_params ())
      {
	BaseCmp::visit (type);
	return;
      }

    for (size_t i = 0; i < base->num_params (); i++)
      {
	auto a = base->param_at (i).second;
	auto b = type.param_at (i).second;

	auto unified_param = a->unify (b);
	if (unified_param->get_kind () == TypeKind::ERROR)
	  {
	    BaseCmp::visit (type);
	    return;
	  }
      }

    auto unified_return
      = base->get_return_type ()->unify (type.get_return_type ());
    if (unified_return->get_kind () == TypeKind::ERROR)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  BaseType *get_base () override { return base; }

  FnType *base;
};

class FnptrCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  FnptrCmp (FnPtr *base) : BaseCmp (base), base (base) {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

  void visit (FnPtr &type) override
  {
    auto this_ret_type = base->get_return_type ();
    auto other_ret_type = type.get_return_type ();
    auto unified_result = this_ret_type->unify (other_ret_type);
    if (unified_result == nullptr
	|| unified_result->get_kind () == TypeKind::ERROR)
      {
	BaseCmp::visit (type);
	return;
      }

    if (base->num_params () != type.num_params ())
      {
	BaseCmp::visit (type);
	return;
      }

    for (size_t i = 0; i < base->num_params (); i++)
      {
	auto this_param = base->param_at (i);
	auto other_param = type.param_at (i);
	auto unified_param = this_param->unify (other_param);
	if (unified_param == nullptr
	    || unified_param->get_kind () == TypeKind::ERROR)
	  {
	    BaseCmp::visit (type);
	    return;
	  }
      }

    ok = true;
  }

  void visit (FnType &type) override
  {
    auto this_ret_type = base->get_return_type ();
    auto other_ret_type = type.get_return_type ();
    auto unified_result = this_ret_type->unify (other_ret_type);
    if (unified_result == nullptr
	|| unified_result->get_kind () == TypeKind::ERROR)
      {
	BaseCmp::visit (type);
	return;
      }

    if (base->num_params () != type.num_params ())
      {
	BaseCmp::visit (type);
	return;
      }

    for (size_t i = 0; i < base->num_params (); i++)
      {
	auto this_param = base->param_at (i);
	auto other_param = type.param_at (i).second;
	auto unified_param = this_param->unify (other_param);
	if (unified_param == nullptr
	    || unified_param->get_kind () == TypeKind::ERROR)
	  {
	    BaseCmp::visit (type);
	    return;
	  }
      }

    ok = true;
  }

private:
  BaseType *get_base () override { return base; }

  FnPtr *base;
};

class ArrayCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ArrayCmp (ArrayType *base) : BaseCmp (base), base (base) {}

  void visit (ArrayType &type) override
  {
    // check base type
    auto base_resolved
      = base->get_element_type ()->unify (type.get_element_type ());
    if (base_resolved->get_kind () == TypeKind::ERROR)
      {
	BaseCmp::visit (type);
	return;
      }

    // need to check the base types and capacity
    if (type.get_capacity () != base->get_capacity ())
      {
	Location locus = mappings->lookup_location (type.get_ref ());
	rust_error_at (locus, "mismatch in array capacity");
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  BaseType *get_base () override { return base; }

  ArrayType *base;
};

class BoolCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  BoolCmp (BoolType *base) : BaseCmp (base), base (base) {}

  void visit (BoolType &type) override { ok = true; }

  void visit (InferType &type) override
  {
    ok = type.get_infer_kind () == InferType::InferTypeKind::GENERAL;
  }

private:
  BaseType *get_base () override { return base; }

  BoolType *base;
};

class IntCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  IntCmp (IntType *base) : BaseCmp (base), base (base) {}

  void visit (InferType &type) override
  {
    ok = type.get_infer_kind () != InferType::InferTypeKind::FLOAT;
  }

  void visit (IntType &type) override
  {
    ok = type.get_int_kind () == base->get_int_kind ();
  }

private:
  BaseType *get_base () override { return base; }

  IntType *base;
};

class UintCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  UintCmp (UintType *base) : BaseCmp (base), base (base) {}

  void visit (InferType &type) override
  {
    ok = type.get_infer_kind () != InferType::InferTypeKind::FLOAT;
  }

  void visit (UintType &type) override
  {
    ok = type.get_uint_kind () == base->get_uint_kind ();
  }

private:
  BaseType *get_base () override { return base; }

  UintType *base;
};

class FloatCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  FloatCmp (FloatType *base) : BaseCmp (base), base (base) {}

  void visit (InferType &type) override
  {
    ok = type.get_infer_kind () != InferType::InferTypeKind::INTEGRAL;
  }

  void visit (FloatType &type) override
  {
    ok = type.get_float_kind () == base->get_float_kind ();
  }

private:
  BaseType *get_base () override { return base; }

  FloatType *base;
};

class ADTCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ADTCmp (ADTType *base) : BaseCmp (base), base (base) {}

  void visit (ADTType &type) override
  {
    if (base->get_identifier ().compare (type.get_identifier ()) != 0)
      {
	BaseCmp::visit (type);
	return;
      }

    if (base->num_fields () != type.num_fields ())
      {
	BaseCmp::visit (type);
	return;
      }

    for (size_t i = 0; i < type.num_fields (); ++i)
      {
	TyTy::StructFieldType *base_field = base->get_field (i);
	TyTy::StructFieldType *other_field = type.get_field (i);

	TyTy::BaseType *this_field_ty = base_field->get_field_type ();
	TyTy::BaseType *other_field_ty = other_field->get_field_type ();

	if (!this_field_ty->can_eq (other_field_ty))
	  {
	    BaseCmp::visit (type);
	    return;
	  }
      }

    ok = true;
  }

private:
  BaseType *get_base () override { return base; }

  ADTType *base;
};

class TupleCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  TupleCmp (TupleType *base) : BaseCmp (base), base (base) {}

  void visit (TupleType &type) override
  {
    if (base->num_fields () != type.num_fields ())
      {
	BaseCmp::visit (type);
	return;
      }

    for (size_t i = 0; i < base->num_fields (); i++)
      {
	BaseType *bo = base->get_field (i);
	BaseType *fo = type.get_field (i);

	if (!bo->can_eq (fo))
	  {
	    BaseCmp::visit (type);
	    return;
	  }
      }

    ok = true;
  }

private:
  BaseType *get_base () override { return base; }

  TupleType *base;
};

class USizeCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  USizeCmp (USizeType *base) : BaseCmp (base), base (base) {}

  void visit (InferType &type) override
  {
    ok = type.get_infer_kind () != InferType::InferTypeKind::FLOAT;
  }

  void visit (USizeType &type) override { ok = true; }

private:
  BaseType *get_base () override { return base; }

  USizeType *base;
};

class ISizeCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ISizeCmp (ISizeType *base) : BaseCmp (base), base (base) {}

  void visit (InferType &type) override
  {
    ok = type.get_infer_kind () != InferType::InferTypeKind::FLOAT;
  }

  void visit (ISizeType &type) override { ok = true; }

private:
  BaseType *get_base () override { return base; }

  ISizeType *base;
};

class CharCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  CharCmp (CharType *base) : BaseCmp (base), base (base) {}

  void visit (InferType &type) override
  {
    ok = type.get_infer_kind () == InferType::InferTypeKind::GENERAL;
  }

  void visit (CharType &type) override { ok = true; }

private:
  BaseType *get_base () override { return base; }

  CharType *base;
};

class ReferenceCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ReferenceCmp (ReferenceType *base) : BaseCmp (base), base (base) {}

  void visit (ReferenceType &type) override
  {
    auto base_type = base->get_base ();
    auto other_base_type = type.get_base ();

    ok = base_type->can_eq (other_base_type);
  }

private:
  BaseType *get_base () override { return base; }

  ReferenceType *base;
};

class ParamCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ParamCmp (ParamType *base) : BaseCmp (base), base (base) {}

  // param types are a placeholder we shouldn't have cases where we unify
  // against it. eg: struct foo<T> { a: T }; When we invoke it we can do either:
  //
  // foo<i32>{ a: 123 }.
  // Then this enforces the i32 type to be referenced on the
  // field via an hirid.
  //
  // rust also allows for a = foo{a:123}; Where we can use an Inference Variable
  // to handle the typing of the struct
  bool can_eq (BaseType *other) override final
  {
    if (base->get_ref () == base->get_ty_ref ())
      return BaseCmp::can_eq (other);

    auto context = Resolver::TypeCheckContext::get ();
    BaseType *lookup = nullptr;
    bool ok = context->lookup_type (base->get_ty_ref (), &lookup);
    rust_assert (ok);

    if (lookup->get_kind () == TypeKind::PARAM)
      {
	InferType infer (UNKNOWN_HIRID, InferType::InferTypeKind::GENERAL);
	return infer.can_eq (other);
      }

    return lookup->can_eq (other);
  }

  // imagine the case where we have:
  // struct Foo<T>(T);
  // Then we declare a generic impl block
  // impl <X>Foo<X> { ... }
  // both of these types are compatible so we mostly care about the number of
  // generic arguments
  void visit (ParamType &type) override { ok = true; }

private:
  BaseType *get_base () override { return base; }

  ParamType *base;
};

class StrCmp : public BaseCmp
{
  // FIXME we will need a enum for the StrType like ByteBuf etc..
  using Rust::TyTy::BaseCmp::visit;

public:
  StrCmp (StrType *base) : BaseCmp (base), base (base) {}

  void visit (StrType &type) override { ok = true; }

private:
  BaseType *get_base () override { return base; }

  StrType *base;
};

class NeverCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  NeverCmp (NeverType *base) : BaseCmp (base), base (base) {}

  void visit (NeverType &type) override { ok = true; }

private:
  BaseType *get_base () override { return base; }

  NeverType *base;
};

class PlaceholderCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  PlaceholderCmp (PlaceholderType *base) : BaseCmp (base), base (base) {}

private:
  BaseType *get_base () override { return base; }

  PlaceholderType *base;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_CMP_H
