// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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
#include "rust-type-util.h"

namespace Rust {
namespace TyTy {

void
emit_unexpected_argument_error (location_t loc,
				unsigned long unexpected_arg_count,
				unsigned long expected_arg_count)
{
  // https://doc.rust-lang.org/error_codes/E0061.html
  // rustc treats 1 as singular and others as plural
  std::string err_msg = "this function takes %lu ";
  if (expected_arg_count == 1)
    {
      err_msg += "argument";
    }
  else
    {
      err_msg += "arguments";
    }

  if (unexpected_arg_count == 1)
    {
      err_msg += " but %lu argument was supplied";
    }
  else
    {
      err_msg += " but %lu arguments were supplied";
    }
  rust_error_at (loc, ErrorCode::E0061, err_msg.c_str (), expected_arg_count,
		 unexpected_arg_count);
}

void
TypeCheckCallExpr::visit (ADTType &type)
{
  rust_assert (!variant.is_error ());
  if (variant.get_variant_type () != TyTy::VariantDef::VariantType::TUPLE)
    {
      rust_error_at (
	call.get_locus (), ErrorCode::E0423,
	"expected function, tuple struct or tuple variant, found struct %qs",
	type.get_name ().c_str ());
      return;
    }

  if (call.num_params () != variant.num_fields ())
    {
      emit_unexpected_argument_error (call.get_locus (),
				      (unsigned long) call.num_params (),
				      (unsigned long) variant.num_fields ());
      return;
    }

  size_t i = 0;
  for (auto &argument : call.get_arguments ())
    {
      StructFieldType *field = variant.get_field_at_index (i);
      BaseType *field_tyty = field->get_field_type ();
      location_t arg_locus = argument->get_locus ();

      BaseType *arg = Resolver::TypeCheckExpr::Resolve (argument.get ());
      if (arg->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (argument->get_locus (),
			 "failed to resolve argument type");
	  return;
	}

      HirId coercion_side_id = argument->get_mappings ().get_hirid ();
      auto res = Resolver::coercion_site (coercion_side_id,
					  TyWithLocation (field_tyty),
					  TyWithLocation (arg, arg_locus),
					  argument->get_locus ());
      if (res->get_kind () == TyTy::TypeKind::ERROR)
	{
	  return;
	}

      i++;
    }

  if (i != call.num_params ())
    {
      emit_unexpected_argument_error (call.get_locus (), (unsigned long) i,
				      (unsigned long) call.num_params ());
      return;
    }

  resolved = type.clone ();
}

void
TypeCheckCallExpr::visit (FnType &type)
{
  if (call.num_params () != type.num_params ())
    {
      if (type.is_variadic ())
	{
	  if (call.num_params () < type.num_params ())
	    {
	      emit_unexpected_argument_error (
		call.get_locus (), (unsigned long) call.num_params (),
		(unsigned long) type.num_params ());
	      return;
	    }
	}
      else
	{
	  emit_unexpected_argument_error (call.get_locus (),
					  (unsigned long) call.num_params (),
					  (unsigned long) type.num_params ());
	  return;
	}
    }

  size_t i = 0;
  for (auto &argument : call.get_arguments ())
    {
      location_t arg_locus = argument->get_locus ();
      auto argument_expr_tyty
	= Resolver::TypeCheckExpr::Resolve (argument.get ());
      if (argument_expr_tyty->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (
	    argument->get_locus (),
	    "failed to resolve type for argument expr in CallExpr");
	  return;
	}

      // it might be a variadic function
      if (i < type.num_params ())
	{
	  auto fnparam = type.param_at (i);
	  HIR::Pattern *fn_param_pattern = fnparam.first;
	  BaseType *param_ty = fnparam.second;
	  location_t param_locus
	    = fn_param_pattern == nullptr
		? mappings->lookup_location (param_ty->get_ref ())
		: fn_param_pattern->get_locus ();

	  HirId coercion_side_id = argument->get_mappings ().get_hirid ();
	  auto resolved_argument_type
	    = Resolver::coercion_site (coercion_side_id,
				       TyWithLocation (param_ty, param_locus),
				       TyWithLocation (argument_expr_tyty,
						       arg_locus),
				       argument->get_locus ());
	  if (resolved_argument_type->get_kind () == TyTy::TypeKind::ERROR)
	    {
	      return;
	    }
	}
      else
	{
	  switch (argument_expr_tyty->get_kind ())
	    {
	    case TyTy::TypeKind::ERROR:
	      return;
	      case TyTy::TypeKind::INT: {
		auto &int_ty
		  = static_cast<TyTy::IntType &> (*argument_expr_tyty);
		if ((int_ty.get_int_kind () == TyTy::IntType::IntKind::I8)
		    || (int_ty.get_int_kind () == TyTy::IntType::IntKind::I16))
		  {
		    rich_location richloc (line_table, arg_locus);
		    richloc.add_fixit_replace (
		      "cast the value to c_int: as c_int");
		    rust_error_at (richloc, ErrorCode::E0617,
				   "expected %<c_int%> variadic argument");
		    return;
		  }
		break;
	      }
	      case TyTy::TypeKind::UINT: {
		auto &uint_ty
		  = static_cast<TyTy::UintType &> (*argument_expr_tyty);
		if ((uint_ty.get_uint_kind () == TyTy::UintType::UintKind::U8)
		    || (uint_ty.get_uint_kind ()
			== TyTy::UintType::UintKind::U16))
		  {
		    rich_location richloc (line_table, arg_locus);
		    richloc.add_fixit_replace (
		      "cast the value to c_uint: as c_uint");
		    rust_error_at (richloc, ErrorCode::E0617,
				   "expected %<c_uint%> variadic argument");
		    return;
		  }
		break;
	      }
	      case TyTy::TypeKind::FLOAT: {
		if (static_cast<TyTy::FloatType &> (*argument_expr_tyty)
		      .get_float_kind ()
		    == TyTy::FloatType::FloatKind::F32)
		  {
		    rich_location richloc (line_table, arg_locus);
		    richloc.add_fixit_replace (
		      "cast the value to c_double: as c_double");
		    rust_error_at (richloc, ErrorCode::E0617,
				   "expected %<c_double%> variadic argument");
		    return;
		  }
		break;
	      }
	      case TyTy::TypeKind::BOOL: {
		rich_location richloc (line_table, arg_locus);
		richloc.add_fixit_replace ("cast the value to c_int: as c_int");
		rust_error_at (arg_locus, ErrorCode::E0617,
			       "expected %<c_int%> variadic argument");
		return;
	      }
	      case TyTy::TypeKind::FNDEF: {
		rust_error_at (
		  arg_locus, ErrorCode::E0617,
		  "unexpected function definition type as variadic "
		  "argument - cast to function pointer");
	      }
	      return;
	    default:
	      break;
	    }
	}

      i++;
    }

  if (i < call.num_params ())
    {
      emit_unexpected_argument_error (call.get_locus (), (unsigned long) i,
				      (unsigned long) call.num_params ());
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
      emit_unexpected_argument_error (call.get_locus (),
				      (unsigned long) call.num_params (),
				      (unsigned long) type.num_params ());
      return;
    }

  size_t i = 0;
  for (auto &argument : call.get_arguments ())
    {
      location_t arg_locus = argument->get_locus ();
      BaseType *fnparam = type.get_param_type_at (i);
      auto argument_expr_tyty
	= Resolver::TypeCheckExpr::Resolve (argument.get ());
      if (argument_expr_tyty->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (
	    argument->get_locus (),
	    "failed to resolve type for argument expr in CallExpr");
	  return;
	}

      auto resolved_argument_type = Resolver::coercion_site (
	argument->get_mappings ().get_hirid (), TyWithLocation (fnparam),
	TyWithLocation (argument_expr_tyty, arg_locus), argument->get_locus ());
      if (resolved_argument_type->get_kind () == TyTy::TypeKind::ERROR)
	{
	  return;
	}

      i++;
    }

  if (i != call.num_params ())
    {
      emit_unexpected_argument_error (call.get_locus (), (unsigned long) i,
				      (unsigned long) call.num_params ());
      return;
    }

  resolved = type.get_return_type ()->monomorphized_clone ();
}

// method call checker

TypeCheckMethodCallExpr::TypeCheckMethodCallExpr (
  Analysis::NodeMapping call_mappings, std::vector<Argument> &args,
  location_t call_locus, location_t receiver_locus,
  TyTy::BaseType *adjusted_self, Resolver::TypeCheckContext *context)
  : call_mappings (call_mappings), arguments (args), call_locus (call_locus),
    receiver_locus (receiver_locus), adjusted_self (adjusted_self),
    context (context), mappings (Analysis::Mappings::get ())
{}

BaseType *
TypeCheckMethodCallExpr::go (FnType *ref, HIR::MethodCallExpr &call,
			     TyTy::BaseType *adjusted_self,
			     Resolver::TypeCheckContext *context)
{
  std::vector<Argument> args;
  for (auto &arg : call.get_arguments ())
    {
      BaseType *argument_expr_tyty
	= Resolver::TypeCheckExpr::Resolve (arg.get ());
      if (argument_expr_tyty->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (arg->get_locus (),
			 "failed to resolve type for argument");
	  return new ErrorType (ref->get_ref ());
	}

      Argument a (arg->get_mappings (), argument_expr_tyty, arg->get_locus ());
      args.push_back (std::move (a));
    }

  TypeCheckMethodCallExpr checker (call.get_mappings (), args,
				   call.get_locus (),
				   call.get_receiver ()->get_locus (),
				   adjusted_self, context);
  return checker.check (*ref);
}

BaseType *
TypeCheckMethodCallExpr::go (FnType *ref, Analysis::NodeMapping call_mappings,
			     std::vector<Argument> &args, location_t call_locus,
			     location_t receiver_locus,
			     TyTy::BaseType *adjusted_self,
			     Resolver::TypeCheckContext *context)
{
  TypeCheckMethodCallExpr checker (call_mappings, args, call_locus,
				   receiver_locus, adjusted_self, context);
  return checker.check (*ref);
}

BaseType *
TypeCheckMethodCallExpr::check (FnType &type)
{
  Resolver::unify_site (call_mappings.get_hirid (),
			TyWithLocation (type.get_self_type ()),
			TyWithLocation (adjusted_self, receiver_locus),
			call_locus);

  // +1 for the receiver self
  size_t num_args_to_call = arguments.size () + 1;
  if (num_args_to_call != type.num_params ())
    {
      emit_unexpected_argument_error (call_locus,
				      (unsigned long) num_args_to_call,
				      (unsigned long) type.num_params ());
      return new ErrorType (type.get_ref ());
    }

  size_t i = 1;
  for (auto &argument : arguments)
    {
      location_t arg_locus = argument.get_locus ();

      auto fnparam = type.param_at (i);
      HIR::Pattern *fn_param_pattern = fnparam.first;
      BaseType *param_ty = fnparam.second;
      location_t param_locus
	= fn_param_pattern == nullptr
	    ? mappings->lookup_location (param_ty->get_ref ())
	    : fn_param_pattern->get_locus ();

      auto argument_expr_tyty = argument.get_argument_type ();
      HirId coercion_side_id = argument.get_mappings ().get_hirid ();
      auto resolved_argument_type = Resolver::coercion_site (
	coercion_side_id, TyWithLocation (param_ty, param_locus),
	TyWithLocation (argument_expr_tyty, arg_locus), arg_locus);
      if (resolved_argument_type->get_kind () == TyTy::TypeKind::ERROR)
	{
	  return new ErrorType (type.get_ref ());
	}

      i++;
    }

  if (i != num_args_to_call)
    {
      emit_unexpected_argument_error (call_locus, (unsigned long) i,
				      (unsigned long) arguments.size ());
      return new ErrorType (type.get_ref ());
    }

  type.monomorphize ();

  return type.get_return_type ()->monomorphized_clone ();
}

} // namespace TyTy
} // namespace Rust
