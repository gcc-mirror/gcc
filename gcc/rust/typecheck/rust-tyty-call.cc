// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#include "rust-tyty-call.h"
#include "rust-hir-type-check-expr.h"

namespace Rust {
namespace TyTy {

void
TypeCheckCallExpr::visit (ADTType &type)
{
  rust_assert (!variant.is_error ());
  if (variant.get_variant_type () != TyTy::VariantDef::VariantType::TUPLE)
    {
      rust_error_at (
	call.get_locus (),
	"expected function, tuple struct or tuple variant, found struct %<%s%>",
	type.get_name ().c_str ());
      return;
    }

  if (call.num_params () != variant.num_fields ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     (unsigned long) call.num_params (),
		     (unsigned long) variant.num_fields ());
      return;
    }

  size_t i = 0;
  for (auto &argument : call.get_arguments ())
    {
      StructFieldType *field = variant.get_field_at_index (i);
      BaseType *field_tyty = field->get_field_type ();
      Location arg_locus = argument->get_locus ();

      BaseType *arg = Resolver::TypeCheckExpr::Resolve (argument.get ());
      if (arg->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (argument->get_locus (),
			 "failed to resolve argument type");
	  return;
	}

      HirId coercion_side_id = argument->get_mappings ().get_hirid ();
      auto res = Resolver::TypeCheckBase::coercion_site (
	coercion_side_id, TyWithLocation (field_tyty),
	TyWithLocation (arg, arg_locus), argument->get_locus ());
      if (res->get_kind () == TyTy::TypeKind::ERROR)
	{
	  return;
	}

      delete res;
      i++;
    }

  if (i != call.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     (unsigned long) i, (unsigned long) call.num_params ());
      return;
    }

  resolved = type.clone ();
}

void
TypeCheckCallExpr::visit (FnType &type)
{
  type.monomorphize ();
  if (call.num_params () != type.num_params ())
    {
      if (type.is_varadic ())
	{
	  if (call.num_params () < type.num_params ())
	    {
	      rust_error_at (call.get_locus (),
			     "unexpected number of arguments %lu expected %lu",
			     (unsigned long) call.num_params (),
			     (unsigned long) type.num_params ());
	      return;
	    }
	}
      else
	{
	  rust_error_at (call.get_locus (),
			 "unexpected number of arguments %lu expected %lu",
			 (unsigned long) call.num_params (),
			 (unsigned long) type.num_params ());
	  return;
	}
    }

  size_t i = 0;
  for (auto &argument : call.get_arguments ())
    {
      Location arg_locus = argument->get_locus ();
      auto argument_expr_tyty
	= Resolver::TypeCheckExpr::Resolve (argument.get ());
      if (argument_expr_tyty->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (
	    argument->get_locus (),
	    "failed to resolve type for argument expr in CallExpr");
	  return;
	}

      // it might be a varadic function
      if (i < type.num_params ())
	{
	  auto fnparam = type.param_at (i);
	  HIR::Pattern *fn_param_pattern = fnparam.first;
	  BaseType *param_ty = fnparam.second;
	  Location param_locus
	    = fn_param_pattern == nullptr
		? mappings->lookup_location (param_ty->get_ref ())
		: fn_param_pattern->get_locus ();

	  HirId coercion_side_id = argument->get_mappings ().get_hirid ();
	  auto resolved_argument_type = Resolver::TypeCheckBase::coercion_site (
	    coercion_side_id, TyWithLocation (param_ty, param_locus),
	    TyWithLocation (argument_expr_tyty, arg_locus),
	    argument->get_locus ());
	  if (resolved_argument_type->get_kind () == TyTy::TypeKind::ERROR)
	    {
	      rust_error_at (argument->get_locus (),
			     "Type Resolution failure on parameter");
	      return;
	    }
	}

      i++;
    }

  if (i < call.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     (unsigned long) i, (unsigned long) call.num_params ());
      return;
    }

  type.monomorphize ();
  resolved = type.get_return_type ()->clone ();
}

void
TypeCheckCallExpr::visit (FnPtr &type)
{
  if (call.num_params () != type.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     (unsigned long) call.num_params (),
		     (unsigned long) type.num_params ());
      return;
    }

  size_t i = 0;
  for (auto &argument : call.get_arguments ())
    {
      Location arg_locus = argument->get_locus ();
      BaseType *fnparam = type.param_at (i);
      auto argument_expr_tyty
	= Resolver::TypeCheckExpr::Resolve (argument.get ());
      if (argument_expr_tyty->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (
	    argument->get_locus (),
	    "failed to resolve type for argument expr in CallExpr");
	  return;
	}

      auto resolved_argument_type = Resolver::TypeCheckBase::coercion_site (
	argument->get_mappings ().get_hirid (), TyWithLocation (fnparam),
	TyWithLocation (argument_expr_tyty, arg_locus), argument->get_locus ());
      if (resolved_argument_type->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (argument->get_locus (),
			 "Type Resolution failure on parameter");
	  return;
	}

      i++;
    }

  if (i != call.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     (unsigned long) i, (unsigned long) call.num_params ());
      return;
    }

  resolved = type.get_return_type ()->monomorphized_clone ();
}

// method call checker

void
TypeCheckMethodCallExpr::visit (FnType &type)
{
  type.get_self_type ()->unify (adjusted_self);

  // +1 for the receiver self
  size_t num_args_to_call = call.num_params () + 1;
  if (num_args_to_call != type.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     (unsigned long) call.num_params (),
		     (unsigned long) type.num_params ());
      return;
    }

  size_t i = 1;
  for (auto &argument : call.get_arguments ())
    {
      Location arg_locus = argument->get_locus ();

      auto fnparam = type.param_at (i);
      HIR::Pattern *fn_param_pattern = fnparam.first;
      BaseType *param_ty = fnparam.second;
      Location param_locus
	= fn_param_pattern == nullptr
	    ? mappings->lookup_location (param_ty->get_ref ())
	    : fn_param_pattern->get_locus ();

      auto argument_expr_tyty
	= Resolver::TypeCheckExpr::Resolve (argument.get ());
      if (argument_expr_tyty->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (
	    argument->get_locus (),
	    "failed to resolve type for argument expr in CallExpr");
	  return;
	}

      HirId coercion_side_id = argument->get_mappings ().get_hirid ();
      auto resolved_argument_type = Resolver::TypeCheckBase::coercion_site (
	coercion_side_id, TyWithLocation (param_ty, param_locus),
	TyWithLocation (argument_expr_tyty, arg_locus), argument->get_locus ());
      if (resolved_argument_type->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (argument->get_locus (),
			 "Type Resolution failure on parameter");
	  return;
	}

      i++;
    }

  if (i != num_args_to_call)
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     (unsigned long) i, (unsigned long) call.num_params ());
      return;
    }

  type.monomorphize ();

  resolved = type.get_return_type ()->monomorphized_clone ();
}

} // namespace TyTy
} // namespace Rust
