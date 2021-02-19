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

BaseType *
UnitType::unify (BaseType *other)
{
  UnitRules r (this);
  return r.unify (other);
}

BaseType *
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

BaseType *
InferType::unify (BaseType *other)
{
  InferRules r (this);
  return r.unify (other);
}

BaseType *
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

BaseType *
ErrorType::unify (BaseType *other)
{
  // FIXME
  // rust_error_at ();
  return this;
}

BaseType *
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

BaseType *
StructFieldType::unify (BaseType *other)
{
  StructFieldTypeRules r (this);
  return r.unify (other);
}

bool
StructFieldType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    {
      return false;
    }
  else
    {
      auto other2 = static_cast<const StructFieldType &> (other);
      return get_field_type () == other2.get_field_type ();
    }
}

BaseType *
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

BaseType *
ADTType::unify (BaseType *other)
{
  ADTRules r (this);
  return r.unify (other);
}

bool
ADTType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    {
      return false;
    }
  else
    {
      auto other2 = static_cast<const ADTType &> (other);
      if (num_fields () != other2.num_fields ())
	{
	  return false;
	}
      for (size_t i = 0; i < num_fields (); i++)
	{
	  if (!get_field (i)->is_equal (*other2.get_field (i)))
	    {
	      return false;
	    }
	}
      return true;
    }
}

BaseType *
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
  iterate_fields ([&] (BaseType *field) mutable -> bool {
    fields_buffer += field->as_string ();
    fields_buffer += ", ";
    return true;
  });
  return "(" + fields_buffer + ")";
}

BaseType *
TupleType::get_field (size_t index) const
{
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (fields.at (index), &lookup);
  rust_assert (ok);
  return lookup;
}

BaseType *
TupleType::unify (BaseType *other)
{
  TupleRules r (this);
  return r.unify (other);
}

bool
TupleType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    {
      return false;
    }
  else
    {
      auto other2 = static_cast<const TupleType &> (other);
      if (num_fields () != other2.num_fields ())
	{
	  return false;
	}
      for (size_t i = 0; i < num_fields (); i++)
	{
	  if (!get_field (i)->is_equal (*other2.get_field (i)))
	    {
	      return false;
	    }
	}
      return true;
    }
}

BaseType *
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

BaseType *
FnType::unify (BaseType *other)
{
  FnRules r (this);
  return r.unify (other);
}

bool
FnType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    {
      return false;
    }
  else
    {
      auto other2 = static_cast<const FnType &> (other);
      if (!get_return_type ()->is_equal (*other2.get_return_type ()))
	return false;
      if (num_params () != other2.num_params ())
	return false;
      for (size_t i = 0; i < num_params (); i++)
	{
	  auto lhs = param_at (i).second;
	  auto rhs = other2.param_at (i).second;
	  if (!lhs->is_equal (*rhs))
	    return false;
	}
      return true;
    }
}

BaseType *
FnType::clone ()
{
  std::vector<std::pair<HIR::Pattern *, BaseType *> > cloned_params;
  for (auto &p : params)
    cloned_params.push_back (
      std::pair<HIR::Pattern *, BaseType *> (p.first, p.second->clone ()));

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

BaseType *
ArrayType::unify (BaseType *other)
{
  ArrayRules r (this);
  return r.unify (other);
}

bool
ArrayType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    {
      return false;
    }
  else
    {
      auto other2 = static_cast<const ArrayType &> (other);
      return get_type () == other2.get_type ()
	     && get_capacity () == other2.get_capacity ();
    }
}

BaseType *
ArrayType::get_type () const
{
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (element_type_id, &lookup);
  rust_assert (ok);
  return lookup;
}

BaseType *
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

BaseType *
BoolType::unify (BaseType *other)
{
  BoolRules r (this);
  return r.unify (other);
}

BaseType *
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

BaseType *
IntType::unify (BaseType *other)
{
  IntRules r (this);
  return r.unify (other);
}

BaseType *
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

BaseType *
UintType::unify (BaseType *other)
{
  UintRules r (this);
  return r.unify (other);
}

BaseType *
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

BaseType *
FloatType::unify (BaseType *other)
{
  FloatRules r (this);
  return r.unify (other);
}

BaseType *
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

BaseType *
USizeType::unify (BaseType *other)
{
  USizeRules r (this);
  return r.unify (other);
}

BaseType *
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

BaseType *
ISizeType::unify (BaseType *other)
{
  ISizeRules r (this);
  return r.unify (other);
}

BaseType *
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

BaseType *
CharType::unify (BaseType *other)
{
  CharRules r (this);
  return r.unify (other);
}

BaseType *
CharType::clone ()
{
  return new CharType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
ReferenceType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ReferenceType::as_string () const
{
  return "&" + get_base ()->as_string ();
}

BaseType *
ReferenceType::unify (BaseType *other)
{
  ReferenceRules r (this);
  return r.unify (other);
}

bool
ReferenceType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    {
      return false;
    }
  else
    {
      auto other2 = static_cast<const ReferenceType &> (other);
      return get_base () == other2.get_base ();
    }
}

const BaseType *
ReferenceType::get_base () const
{
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (base, &lookup);
  rust_assert (ok);
  return lookup;
}

BaseType *
ReferenceType::get_base ()
{
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (base, &lookup);
  rust_assert (ok);
  return lookup;
}

BaseType *
ReferenceType::clone ()
{
  return new ReferenceType (get_ref (), get_ty_ref (), base,
			    get_combined_refs ());
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
    BaseType *field_tyty = field->get_field_type ();

    BaseType *arg = Resolver::TypeCheckExpr::Resolve (p, false);
    if (arg == nullptr)
      {
	rust_error_at (p->get_locus_slow (), "failed to resolve argument type");
	return false;
      }

    auto res = field_tyty->unify (arg);
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

    auto resolved_argument_type = fnparam.second->unify (argument_expr_tyty);
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

    auto resolved_argument_type = fnparam.second->unify (argument_expr_tyty);
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
