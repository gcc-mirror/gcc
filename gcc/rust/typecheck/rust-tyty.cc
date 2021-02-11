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
  return new UnitType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
InferType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
InferType::as_string () const
{
  switch (infer_kind)
    {
    case GENERAL:
      return "T?";
    case INTEGRAL:
      return "<integer>";
    case FLOAT:
      return "<float>";
    }
  return "<infer::error>";
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
  return new InferType (get_ref (), get_ty_ref (), get_infer_kind (),
			get_combined_refs ());
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
  return new ErrorType (get_ref (), get_ty_ref (), get_combined_refs ());
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
			      get_field_type ()->clone (),
			      get_combined_refs ());
}

void
ADTType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ADTType::as_string () const
{
  // if (num_fields () == 0)
  //   return identifier;

  // std::string fields_buffer;
  // for (auto &field : fields)
  //   fields_buffer += field->as_string () + ", ";

  // return identifier + "{" + fields_buffer + "}";
  return identifier;
}

TyBase *
ADTType::combine (TyBase *other)
{
  ADTRules r (this);
  return r.combine (other);
}

TyBase *
ADTType::clone ()
{
  std::vector<StructFieldType *> cloned_fields;
  for (auto &f : fields)
    cloned_fields.push_back ((StructFieldType *) f->clone ());

  return new ADTType (get_ref (), get_ty_ref (), get_name (), cloned_fields,
		      get_combined_refs ());
}

void
TupleType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
TupleType::as_string () const
{
  std::string fields_buffer;
  iterate_fields ([&] (TyBase *field) mutable -> bool {
    fields_buffer += field->as_string ();
    fields_buffer += ", ";
    return true;
  });
  return "(" + fields_buffer + ")";
}

TyBase *
TupleType::get_field (size_t index) const
{
  auto context = Resolver::TypeCheckContext::get ();
  TyBase *lookup = nullptr;
  bool ok = context->lookup_type (fields.at (index), &lookup);
  rust_assert (ok);
  return lookup;
}

TyBase *
TupleType::combine (TyBase *other)
{
  TupleRules r (this);
  return r.combine (other);
}

TyBase *
TupleType::clone ()
{
  return new TupleType (get_ref (), get_ty_ref (), fields,
			get_combined_refs ());
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
      auto pattern = param.first;
      auto ty = param.second;
      params_str += pattern->as_string () + " " + ty->as_string ();
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
  std::vector<std::pair<HIR::Pattern *, TyBase *> > cloned_params;
  for (auto &p : params)
    cloned_params.push_back (
      std::pair<HIR::Pattern *, TyBase *> (p.first, p.second->clone ()));

  return new FnType (get_ref (), get_ty_ref (), cloned_params,
		     get_return_type ()->clone (), get_combined_refs ());
}

void
ArrayType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ArrayType::as_string () const
{
  return "[" + get_type ()->as_string () + ":" + std::to_string (capacity)
	 + "]";
}

TyBase *
ArrayType::combine (TyBase *other)
{
  ArrayRules r (this);
  return r.combine (other);
}

TyBase *
ArrayType::get_type () const
{
  auto context = Resolver::TypeCheckContext::get ();
  TyBase *lookup = nullptr;
  bool ok = context->lookup_type (element_type_id, &lookup);
  rust_assert (ok);
  return lookup;
}

TyBase *
ArrayType::clone ()
{
  return new ArrayType (get_ref (), get_ty_ref (), get_capacity (),
			get_type ()->clone (), get_combined_refs ());
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
  return new BoolType (get_ref (), get_ty_ref (), get_combined_refs ());
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
  return new IntType (get_ref (), get_ty_ref (), get_kind (),
		      get_combined_refs ());
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
  return new UintType (get_ref (), get_ty_ref (), get_kind (),
		       get_combined_refs ());
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
  return new FloatType (get_ref (), get_ty_ref (), get_kind (),
			get_combined_refs ());
}

void
USizeType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
USizeType::as_string () const
{
  return "usize";
}

TyBase *
USizeType::combine (TyBase *other)
{
  USizeRules r (this);
  return r.combine (other);
}

TyBase *
USizeType::clone ()
{
  return new USizeType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
ISizeType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ISizeType::as_string () const
{
  return "isize";
}

TyBase *
ISizeType::combine (TyBase *other)
{
  ISizeRules r (this);
  return r.combine (other);
}

TyBase *
ISizeType::clone ()
{
  return new ISizeType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
CharType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
CharType::as_string () const
{
  return "char";
}

TyBase *
CharType::combine (TyBase *other)
{
  CharRules r (this);
  return r.combine (other);
}

TyBase *
CharType::clone ()
{
  return new CharType (get_ref (), get_ty_ref (), get_combined_refs ());
}

// rust-tyty-call.h

void
TypeCheckCallExpr::visit (ADTType &type)
{
  if (call.num_params () != type.num_fields ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     call.num_params (), type.num_fields ());
      return;
    }

  size_t i = 0;
  call.iterate_params ([&] (HIR::Expr *p) mutable -> bool {
    StructFieldType *field = type.get_field (i);
    TyBase *field_tyty = field->get_field_type ();

    TyBase *arg = Resolver::TypeCheckExpr::Resolve (p, false);
    if (arg == nullptr)
      {
	rust_error_at (p->get_locus_slow (), "failed to resolve argument type");
	return false;
      }

    auto res = field_tyty->combine (arg);
    if (res == nullptr)
      return false;

    delete res;
    i++;
    return true;
  });

  if (i != call.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu", i,
		     call.num_params ());
      return;
    }

  resolved = type.clone ();
}

void
TypeCheckCallExpr::visit (FnType &type)
{
  if (call.num_params () != type.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     call.num_params (), type.num_params ());
      return;
    }

  size_t i = 0;
  call.iterate_params ([&] (HIR::Expr *param) mutable -> bool {
    auto fnparam = type.param_at (i);
    auto argument_expr_tyty = Resolver::TypeCheckExpr::Resolve (param, false);
    if (argument_expr_tyty == nullptr)
      {
	rust_error_at (param->get_locus_slow (),
		       "failed to resolve type for argument expr in CallExpr");
	return false;
      }

    auto resolved_argument_type = fnparam.second->combine (argument_expr_tyty);
    if (resolved_argument_type == nullptr)
      {
	rust_error_at (param->get_locus_slow (),
		       "Type Resolution failure on parameter");
	return false;
      }

    context->insert_type (param->get_mappings (), resolved_argument_type);

    i++;
    return true;
  });

  if (i != call.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu", i,
		     call.num_params ());
      return;
    }

  resolved = type.get_return_type ()->clone ();
}

// method call checker

void
TypeCheckMethodCallExpr::visit (FnType &type)
{
  // +1 for the receiver self
  size_t num_args_to_call = call.num_params () + 1;
  if (num_args_to_call != type.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     call.num_params (), type.num_params ());
      return;
    }

  size_t i = 1;
  call.iterate_params ([&] (HIR::Expr *param) mutable -> bool {
    auto fnparam = type.param_at (i);
    auto argument_expr_tyty = Resolver::TypeCheckExpr::Resolve (param, false);
    if (argument_expr_tyty == nullptr)
      {
	rust_error_at (param->get_locus_slow (),
		       "failed to resolve type for argument expr in CallExpr");
	return false;
      }

    auto resolved_argument_type = fnparam.second->combine (argument_expr_tyty);
    if (resolved_argument_type == nullptr)
      {
	rust_error_at (param->get_locus_slow (),
		       "Type Resolution failure on parameter");
	return false;
      }

    context->insert_type (param->get_mappings (), resolved_argument_type);

    i++;
    return true;
  });

  if (i != num_args_to_call)
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu", i,
		     call.num_params ());
      return;
    }

  resolved = type.get_return_type ()->clone ();
}

} // namespace TyTy
} // namespace Rust
