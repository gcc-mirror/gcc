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
#include "rust-hir-type-check-type.h"
#include "rust-tyty-rules.h"
#include "rust-hir-map.h"

namespace Rust {
namespace TyTy {

TyCtx::TyCtx (HirId ref) : ref (ref)
{
  // ensure this reference is defined within the context
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (ref, &lookup);
  rust_assert (ok);
}

BaseType *
TyCtx::get_tyty () const
{
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (ref, &lookup);
  rust_assert (ok);
  return lookup;
}

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
  return this;
}

BaseType *
ErrorType::clone ()
{
  return new ErrorType (get_ref (), get_ty_ref (), get_combined_refs ());
}

std::string
StructFieldType::as_string () const
{
  return name + ":" + ty->as_string ();
}

bool
StructFieldType::is_equal (const StructFieldType &other) const
{
  return get_name ().compare (other.get_name ()) == 0
	 && get_field_type ()->is_equal (*other.get_field_type ());
}

StructFieldType *
StructFieldType::clone () const
{
  return new StructFieldType (get_ref (), get_name (),
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
  if (num_fields () == 0)
    return identifier;

  std::string fields_buffer;
  for (size_t i = 0; i < num_fields (); ++i)
    {
      fields_buffer += get_field (i)->as_string ();
      if ((i + 1) < num_fields ())
	fields_buffer += ", ";
    }

  return identifier + subst_as_string () + "{" + fields_buffer + "}";
}

const StructFieldType *
ADTType::get_field (size_t index) const
{
  return fields.at (index);
}

const BaseType *
ADTType::get_field_type (size_t index) const
{
  const StructFieldType *ref = get_field (index);
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (ref->get_field_type ()->get_ref (), &lookup);
  rust_assert (ok);
  return lookup;
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
    return false;

  auto other2 = static_cast<const ADTType &> (other);
  if (num_fields () != other2.num_fields ())
    return false;

  for (size_t i = 0; i < num_fields (); i++)
    {
      if (!get_field (i)->is_equal (*other2.get_field (i)))
	return false;
    }

  return true;
}

BaseType *
ADTType::clone ()
{
  std::vector<StructFieldType *> cloned_fields;
  for (auto &f : fields)
    cloned_fields.push_back ((StructFieldType *) f->clone ());

  return new ADTType (get_ref (), get_ty_ref (), identifier, get_is_tuple (),
		      cloned_fields, clone_substs (), get_combined_refs ());
}

ADTType *
ADTType::infer_substitions ()
{
  auto context = Resolver::TypeCheckContext::get ();
  ADTType *adt = static_cast<ADTType *> (clone ());

  for (auto &sub : adt->get_substs ())
    {
      // generate an new inference variable
      InferType *infer = new InferType (mappings->get_next_hir_id (),
					InferType::InferTypeKind::GENERAL);
      context->insert_type (
	Analysis::NodeMapping (mappings->get_current_crate (), UNKNOWN_NODEID,
			       infer->get_ref (), UNKNOWN_LOCAL_DEFID),
	infer);

      sub.fill_param_ty (infer);
      adt->fill_in_params_for (sub, infer);
    }

  // generate new ty ref id since this is an instantiate of the generic
  adt->set_ty_ref (mappings->get_next_hir_id ());

  return adt;
}

ADTType *
ADTType::handle_substitions (HIR::GenericArgs &generic_args)
{
  if (generic_args.get_type_args ().size () != get_num_substitions ())
    {
      rust_error_at (generic_args.get_locus (),
		     "invalid number of generic arguments to generic ADT type");
      return nullptr;
    }

  ADTType *adt = static_cast<ADTType *> (clone ());
  size_t index = 0;
  for (auto &arg : generic_args.get_type_args ())
    {
      BaseType *resolved = Resolver::TypeCheckType::Resolve (arg.get ());
      if (resolved == nullptr)
	{
	  rust_error_at (generic_args.get_locus (),
			 "failed to resolve type arguments");
	  return nullptr;
	}

      adt->fill_in_at (index, resolved);
      index++;
    }

  // generate new ty ref id since this is an instantiate of the generic
  adt->set_ty_ref (mappings->get_next_hir_id ());

  return adt;
}

void
ADTType::fill_in_at (size_t index, BaseType *type)
{
  SubstitionMapping sub = get_substition_mapping_at (index);
  SubstitionRef<ADTType>::fill_in_at (index, type);
  fill_in_params_for (sub, type);
}

void
ADTType::fill_in_params_for (SubstitionMapping sub, BaseType *type)
{
  iterate_fields ([&] (StructFieldType *field) mutable -> bool {
    bool is_param_ty = field->get_field_type ()->get_kind () == TypeKind::PARAM;
    if (!is_param_ty)
      return true;

    const ParamType *pp = sub.get_param_ty ();
    ParamType *p = static_cast<ParamType *> (field->get_field_type ());

    // for now let just see what symbols match up for the substitution
    if (p->get_symbol ().compare (pp->get_symbol ()) == 0)
      p->set_ty_ref (type->get_ref ());

    return true;
  });
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
  return fields.at (index).get_tyty ();
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
    return false;

  auto other2 = static_cast<const TupleType &> (other);
  if (num_fields () != other2.num_fields ())
    return false;

  for (size_t i = 0; i < num_fields (); i++)
    {
      if (!get_field (i)->is_equal (*other2.get_field (i)))
	return false;
    }
  return true;
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
    return false;

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

BaseType *
FnType::clone ()
{
  std::vector<std::pair<HIR::Pattern *, BaseType *> > cloned_params;
  for (auto &p : params)
    cloned_params.push_back (
      std::pair<HIR::Pattern *, BaseType *> (p.first, p.second->clone ()));

  return new FnType (get_ref (), get_ty_ref (), std::move (cloned_params),
		     get_return_type ()->clone (), get_combined_refs ());
}

void
FnPtr::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
FnPtr::as_string () const
{
  std::string params_str;
  iterate_params ([&] (BaseType *p) mutable -> bool {
    params_str += p->as_string () + " ,";
    return true;
  });
  return "fnptr (" + params_str + ") -> " + get_return_type ()->as_string ();
}

BaseType *
FnPtr::unify (BaseType *other)
{
  FnptrRules r (this);
  return r.unify (other);
}

bool
FnPtr::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    return false;

  auto other2 = static_cast<const FnPtr &> (other);
  auto this_ret_type = get_return_type ();
  auto other_ret_type = other2.get_return_type ();
  if (this_ret_type->is_equal (*other_ret_type))
    return false;

  if (num_params () != other2.num_params ())
    return false;

  for (size_t i = 0; i < num_params (); i++)
    {
      if (!param_at (i)->is_equal (*other2.param_at (i)))
	return false;
    }
  return true;
}

BaseType *
FnPtr::clone ()
{
  std::vector<TyCtx> cloned_params;
  for (auto &p : params)
    cloned_params.push_back (TyCtx (p.get_ref ()));

  return new FnPtr (get_ref (), get_ty_ref (), std::move (cloned_params),
		    result_type, get_combined_refs ());
}

void
ArrayType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ArrayType::as_string () const
{
  return "[" + get_element_type ()->as_string () + ":"
	 + std::to_string (capacity) + "]";
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
    return false;

  auto other2 = static_cast<const ArrayType &> (other);
  if (get_capacity () != other2.get_capacity ())
    return false;

  auto this_element_type = get_element_type ();
  auto other_element_type = other2.get_element_type ();

  return this_element_type->is_equal (*other_element_type);
}

BaseType *
ArrayType::get_element_type () const
{
  return element_type.get_tyty ();
}

BaseType *
ArrayType::clone ()
{
  return new ArrayType (get_ref (), get_ty_ref (), get_capacity (),
			element_type, get_combined_refs ());
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
    return false;

  auto other2 = static_cast<const ReferenceType &> (other);
  return get_base ()->is_equal (*other2.get_base ());
}

BaseType *
ReferenceType::get_base () const
{
  return base.get_tyty ();
}

BaseType *
ReferenceType::clone ()
{
  return new ReferenceType (get_ref (), get_ty_ref (), base,
			    get_combined_refs ());
}

void
ParamType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ParamType::as_string () const
{
  if (get_ref () == get_ty_ref ())
    return get_symbol ();

  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (get_ty_ref (), &lookup);
  rust_assert (ok);

  return lookup->as_string ();
}

BaseType *
ParamType::unify (BaseType *other)
{
  ParamRules r (this);
  return r.unify (other);
}

BaseType *
ParamType::clone ()
{
  return new ParamType (get_symbol (), get_ref (), get_ty_ref (),
			get_generic_param (), get_combined_refs ());
}

std::string
ParamType::get_symbol () const
{
  return symbol;
}

BaseType *
ParamType::resolve ()
{
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (get_ty_ref (), &lookup);
  rust_assert (ok);

  return lookup;
}

BaseType *
StrType::clone ()
{
  return new StrType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
StrType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
StrType::as_string () const
{
  return "str";
}

BaseType *
StrType::unify (BaseType *other)
{
  StrRules r (this);
  return r.unify (other);
}

bool
StrType::is_equal (const BaseType &other) const
{
  return get_kind () == other.get_kind ();
}

// rust-tyty-call.h

void
TypeCheckCallExpr::visit (ADTType &type)
{
  if (!type.get_is_tuple ())
    {
      rust_error_at (
	call.get_locus (),
	"expected function, tuple struct or tuple variant, found struct `%s`",
	type.get_name ().c_str ());
      return;
    }

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

void
TypeCheckCallExpr::visit (FnPtr &type)
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

    auto resolved_argument_type = fnparam->unify (argument_expr_tyty);
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
