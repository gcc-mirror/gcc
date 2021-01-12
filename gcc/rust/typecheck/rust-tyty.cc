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

#include "rust-tyty.h"
#include "rust-tyty-visitor.h"
#include "rust-tyty-call.h"
#include "rust-hir-type-check-expr.h"
#include "rust-tyty-rules.h"

namespace Rust {
namespace TyTy {

void
UnitType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
UnitType::as_string () const
{
  return "()";
}

TyBase *
UnitType::combine (TyBase *other)
{
  UnitRules r (this);
  return r.combine (other);
}

TyBase *
UnitType::clone ()
{
  return new UnitType (get_ref (), get_ty_ref ());
}

void
InferType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
InferType::as_string () const
{
  return "?";
}

TyBase *
InferType::combine (TyBase *other)
{
  InferRules r (this);
  return r.combine (other);
}

TyBase *
InferType::clone ()
{
  return new InferType (get_ref (), get_ty_ref ());
}

void
ErrorType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ErrorType::as_string () const
{
  return "<tyty::error>";
}

TyBase *
ErrorType::combine (TyBase *other)
{
  // FIXME
  // rust_error_at ();
  return this;
}

TyBase *
ErrorType::clone ()
{
  return new ErrorType (get_ref ());
}

void
StructFieldType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
StructFieldType::as_string () const
{
  return name + ":" + ty->as_string ();
}

TyBase *
StructFieldType::combine (TyBase *other)
{
  StructFieldTypeRules r (this);
  return r.combine (other);
}

TyBase *
StructFieldType::clone ()
{
  return new StructFieldType (get_ref (), get_ty_ref (), get_name (),
			      get_field_type ()->clone ());
}

void
ADTType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ADTType::as_string () const
{
  std::string fields_buffer;
  for (auto &field : fields)
    fields_buffer += field->as_string () + "\n";

  return identifier + "{\n" + fields_buffer + "\n}";
}

TyBase *
ADTType::combine (TyBase *other)
{
  return nullptr;
}

TyBase *
ADTType::clone ()
{
  std::vector<StructFieldType *> cloned_fields;
  for (auto &f : fields)
    cloned_fields.push_back ((StructFieldType *) f->clone ());

  return new ADTType (get_ref (), get_ty_ref (), get_name (), cloned_fields);
}

void
FnType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
FnType::as_string () const
{
  std::string params_str = "";
  for (auto &param : params)
    {
      params_str += param->as_string ();
      params_str += ",";
    }

  std::string ret_str = type->as_string ();
  return "fn (" + params_str + ") -> " + ret_str;
}

TyBase *
FnType::combine (TyBase *other)
{
  FnRules r (this);
  return r.combine (other);
}

TyBase *
FnType::clone ()
{
  std::vector<ParamType *> cloned_params;
  for (auto &p : params)
    cloned_params.push_back ((ParamType *) p->clone ());

  return new FnType (get_ref (), get_ty_ref (), cloned_params,
		     get_return_type ()->clone ());
}

void
ParamType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ParamType::as_string () const
{
  return "(" + identifier + " :" + type->as_string () + ")";
}

TyBase *
ParamType::combine (TyBase *other)
{
  ParamRules r (this);
  return r.combine (other);
}

TyBase *
ParamType::clone ()
{
  return new ParamType (get_ref (), get_ty_ref (), get_identifier (),
			get_base_type ()->clone ());
}

void
ArrayType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ArrayType::as_string () const
{
  return "[" + type->as_string () + ":" + std::to_string (capacity) + "]";
}

TyBase *
ArrayType::combine (TyBase *other)
{
  ArrayRules r (this);
  return r.combine (other);
}

TyBase *
ArrayType::clone ()
{
  return new ArrayType (get_ref (), get_ty_ref (), get_capacity (),
			get_type ()->clone ());
}

void
BoolType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
BoolType::as_string () const
{
  return "bool";
}

TyBase *
BoolType::combine (TyBase *other)
{
  BoolRules r (this);
  return r.combine (other);
}

TyBase *
BoolType::clone ()
{
  return new BoolType (get_ref (), get_ty_ref ());
}

void
IntType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
IntType::as_string () const
{
  switch (int_kind)
    {
    case I8:
      return "i8";
    case I16:
      return "i16";
    case I32:
      return "i32";
    case I64:
      return "i64";
    case I128:
      return "i128";
    }
  gcc_unreachable ();
  return "__unknown_int_type";
}

TyBase *
IntType::combine (TyBase *other)
{
  IntRules r (this);
  return r.combine (other);
}

TyBase *
IntType::clone ()
{
  return new IntType (get_ref (), get_ty_ref (), get_kind ());
}

void
UintType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
UintType::as_string () const
{
  switch (uint_kind)
    {
    case U8:
      return "u8";
    case U16:
      return "u16";
    case U32:
      return "u32";
    case U64:
      return "u64";
    case U128:
      return "u128";
    }
  gcc_unreachable ();
  return "__unknown_uint_type";
}

TyBase *
UintType::combine (TyBase *other)
{
  UintRules r (this);
  return r.combine (other);
}

TyBase *
UintType::clone ()
{
  return new UintType (get_ref (), get_ty_ref (), get_kind ());
}

void
FloatType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
FloatType::as_string () const
{
  switch (float_kind)
    {
    case F32:
      return "f32";
    case F64:
      return "f64";
    }
  gcc_unreachable ();
  return "__unknown_float_type";
}

TyBase *
FloatType::combine (TyBase *other)
{
  FloatRules r (this);
  return r.combine (other);
}

TyBase *
FloatType::clone ()
{
  return new FloatType (get_ref (), get_ty_ref (), get_kind ());
}

void
TypeCheckCallExpr::visit (FnType &type)
{
  if (call.num_params () != type.num_params ())
    {
      rust_error_at (call.get_locus (), "differing number of arguments");
      return;
    }

  size_t i = 0;
  call.iterate_params ([&] (HIR::Expr *p) mutable -> bool {
    TyBase *pt = type.param_at (i);
    auto t = Resolver::TypeCheckExpr::Resolve (p);
    if (t == nullptr)
      {
	rust_error_at (p->get_locus_slow (), "failed to resolve type");
	return false;
      }

    auto res = pt->combine (t);
    if (res == nullptr)
      return false;

    i++;
    return true;
  });

  if (i != call.num_params ())
    return;

  resolved = type.get_return_type ();
}

} // namespace TyTy
} // namespace Rust
