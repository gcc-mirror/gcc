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

void
InferType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
InferType::as_string () const
{
  return "[_]";
}

TyBase *
InferType::combine (TyBase *other)
{
  InferRules r (this);
  return r.combine (other);
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
