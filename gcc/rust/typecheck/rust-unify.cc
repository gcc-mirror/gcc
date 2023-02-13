// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-unify.h"

namespace Rust {
namespace Resolver {

UnifyRules::UnifyRules (TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
			Location locus, bool commit_flag, bool emit_error)
  : lhs (lhs), rhs (rhs), locus (locus), commit_flag (commit_flag),
    emit_error (emit_error), mappings (*Analysis::Mappings::get ()),
    context (*TypeCheckContext::get ())
{}

TyTy::BaseType *
UnifyRules::Resolve (TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
		     Location locus, bool commit_flag, bool emit_error)
{
  UnifyRules r (lhs, rhs, locus, commit_flag, emit_error);
  TyTy::BaseType *result = r.go ();

  if (r.commit_flag)
    r.commit (result);

  bool failed = result->get_kind () == TyTy::TypeKind::ERROR;
  if (failed && r.emit_error)
    r.emit_type_mismatch ();

  return result;
}

TyTy::BaseType *
UnifyRules::get_base ()
{
  return lhs.get_ty ()->destructure ();
}

TyTy::BaseType *
UnifyRules::get_other ()
{
  return rhs.get_ty ()->destructure ();
}

void
UnifyRules::commit (TyTy::BaseType *resolved)
{
  resolved->append_reference (get_base ()->get_ref ());
  resolved->append_reference (get_other ()->get_ref ());
  for (auto ref : get_base ()->get_combined_refs ())
    resolved->append_reference (ref);
  for (auto ref : get_other ()->get_combined_refs ())
    resolved->append_reference (ref);

  get_other ()->append_reference (resolved->get_ref ());
  get_other ()->append_reference (get_base ()->get_ref ());
  get_base ()->append_reference (resolved->get_ref ());
  get_base ()->append_reference (get_other ()->get_ref ());

  bool result_resolved = resolved->get_kind () != TyTy::TypeKind::INFER;
  bool result_is_infer_var = resolved->get_kind () == TyTy::TypeKind::INFER;
  bool results_is_non_general_infer_var
    = (result_is_infer_var
       && (static_cast<TyTy::InferType *> (resolved))->get_infer_kind ()
	    != TyTy::InferType::GENERAL);
  if (result_resolved || results_is_non_general_infer_var)
    {
      for (auto &ref : resolved->get_combined_refs ())
	{
	  TyTy::BaseType *ref_tyty = nullptr;
	  bool ok = context.lookup_type (ref, &ref_tyty);
	  if (!ok)
	    continue;

	  // if any of the types are inference variables lets fix them
	  if (ref_tyty->get_kind () == TyTy::TypeKind::INFER)
	    {
	      auto node = Analysis::NodeMapping (mappings.get_current_crate (),
						 UNKNOWN_NODEID, ref,
						 UNKNOWN_LOCAL_DEFID);
	      context.insert_type (node, resolved->clone ());
	    }
	}
    }
}

void
UnifyRules::emit_type_mismatch () const
{
  TyTy::BaseType *expected = lhs.get_ty ();
  TyTy::BaseType *expr = rhs.get_ty ();

  RichLocation r (locus);
  r.add_range (lhs.get_locus ());
  r.add_range (rhs.get_locus ());
  rust_error_at (r, "expected %<%s%> got %<%s%>",
		 expected->get_name ().c_str (), expr->get_name ().c_str ());
}

TyTy::BaseType *
UnifyRules::go ()
{
  TyTy::BaseType *ltype = lhs.get_ty ();
  TyTy::BaseType *rtype = rhs.get_ty ();

  ltype = lhs.get_ty ()->destructure ();
  rtype = rhs.get_ty ()->destructure ();

  rust_debug ("unify::go ltype={%s} rtype={%s}", ltype->debug_str ().c_str (),
	      rtype->debug_str ().c_str ());

  // check bounds
  if (ltype->num_specified_bounds () > 0)
    {
      if (!ltype->bounds_compatible (*rtype, locus, true))
	{
	  // already emitted an error
	  emit_error = false;
	  return new TyTy::ErrorType (0);
	}
    }

  switch (ltype->get_kind ())
    {
    case TyTy::INFER:
      return expect_inference_variable (static_cast<TyTy::InferType *> (ltype),
					rtype);

    case TyTy::ADT:
      return expect_adt (static_cast<TyTy::ADTType *> (ltype), rtype);

    case TyTy::STR:
      return expect_str (static_cast<TyTy::StrType *> (ltype), rtype);

    case TyTy::REF:
      return expect_reference (static_cast<TyTy::ReferenceType *> (ltype),
			       rtype);

    case TyTy::POINTER:
      return expect_pointer (static_cast<TyTy::PointerType *> (ltype), rtype);

    case TyTy::PARAM:
      return expect_param (static_cast<TyTy::ParamType *> (ltype), rtype);

    case TyTy::ARRAY:
      return expect_array (static_cast<TyTy::ArrayType *> (ltype), rtype);

    case TyTy::SLICE:
      return expect_slice (static_cast<TyTy::SliceType *> (ltype), rtype);

    case TyTy::FNDEF:
      return expect_fndef (static_cast<TyTy::FnType *> (ltype), rtype);

    case TyTy::FNPTR:
      return expect_fnptr (static_cast<TyTy::FnPtr *> (ltype), rtype);

    case TyTy::TUPLE:
      return expect_tuple (static_cast<TyTy::TupleType *> (ltype), rtype);

    case TyTy::BOOL:
      return expect_bool (static_cast<TyTy::BoolType *> (ltype), rtype);

    case TyTy::CHAR:
      return expect_char (static_cast<TyTy::CharType *> (ltype), rtype);

    case TyTy::INT:
      return expect_int (static_cast<TyTy::IntType *> (ltype), rtype);

    case TyTy::UINT:
      return expect_uint (static_cast<TyTy::UintType *> (ltype), rtype);

    case TyTy::FLOAT:
      return expect_float (static_cast<TyTy::FloatType *> (ltype), rtype);

    case TyTy::USIZE:
      return expect_usize (static_cast<TyTy::USizeType *> (ltype), rtype);

    case TyTy::ISIZE:
      return expect_isize (static_cast<TyTy::ISizeType *> (ltype), rtype);

    case TyTy::NEVER:
      return expect_never (static_cast<TyTy::NeverType *> (ltype), rtype);

    case TyTy::PLACEHOLDER:
      return expect_placeholder (static_cast<TyTy::PlaceholderType *> (ltype),
				 rtype);

    case TyTy::PROJECTION:
      return expect_projection (static_cast<TyTy::ProjectionType *> (ltype),
				rtype);

    case TyTy::DYNAMIC:
      return expect_dyn (static_cast<TyTy::DynamicObjectType *> (ltype), rtype);

    case TyTy::CLOSURE:
      return expect_closure (static_cast<TyTy::ClosureType *> (ltype), rtype);

    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }

  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_inference_variable (TyTy::InferType *ltype,
				       TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	switch (ltype->get_infer_kind ())
	  {
	  case TyTy::InferType::InferTypeKind::GENERAL:
	    return rtype->clone ();

	    case TyTy::InferType::InferTypeKind::INTEGRAL: {
	      bool is_valid = r->get_infer_kind ()
				== TyTy::InferType::InferTypeKind::INTEGRAL
			      || r->get_infer_kind ()
				   == TyTy::InferType::InferTypeKind::GENERAL;
	      if (is_valid)
		return rtype->clone ();
	    }
	    break;

	    case TyTy::InferType::InferTypeKind::FLOAT: {
	      bool is_valid
		= r->get_infer_kind () == TyTy::InferType::InferTypeKind::FLOAT
		  || r->get_infer_kind ()
		       == TyTy::InferType::InferTypeKind::GENERAL;
	      if (is_valid)
		return rtype->clone ();
	    }
	    break;
	  }
      }
      break;

    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::USIZE:
      case TyTy::ISIZE: {
	bool is_valid = (ltype->get_infer_kind ()
			 == TyTy::InferType::InferTypeKind::GENERAL)
			|| (ltype->get_infer_kind ()
			    == TyTy::InferType::InferTypeKind::INTEGRAL);
	if (is_valid)
	  return rtype->clone ();
      }
      break;

      case TyTy::FLOAT: {
	bool is_valid = (ltype->get_infer_kind ()
			 == TyTy::InferType::InferTypeKind::GENERAL)
			|| (ltype->get_infer_kind ()
			    == TyTy::InferType::InferTypeKind::FLOAT);
	if (is_valid)
	  return rtype->clone ();
      }
      break;

    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
      case TyTy::CLOSURE: {
	bool is_valid = (ltype->get_infer_kind ()
			 == TyTy::InferType::InferTypeKind::GENERAL);
	if (is_valid)
	  return rtype->clone ();
      }
      break;

    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }

  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_adt (TyTy::ADTType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::ADT: {
	TyTy::ADTType &type = *static_cast<TyTy::ADTType *> (rtype);
	if (ltype->get_adt_kind () != type.get_adt_kind ())
	  {
	    return new TyTy::ErrorType (0);
	  }

	if (ltype->get_identifier ().compare (type.get_identifier ()) != 0)
	  {
	    return new TyTy::ErrorType (0);
	  }

	if (ltype->number_of_variants () != type.number_of_variants ())
	  {
	    return new TyTy::ErrorType (0);
	  }

	for (size_t i = 0; i < type.number_of_variants (); ++i)
	  {
	    TyTy::VariantDef *a = ltype->get_variants ().at (i);
	    TyTy::VariantDef *b = type.get_variants ().at (i);

	    if (a->num_fields () != b->num_fields ())
	      {
		return new TyTy::ErrorType (0);
	      }

	    for (size_t j = 0; j < a->num_fields (); j++)
	      {
		TyTy::StructFieldType *base_field = a->get_field_at_index (j);
		TyTy::StructFieldType *other_field = b->get_field_at_index (j);

		TyTy::BaseType *this_field_ty = base_field->get_field_type ();
		TyTy::BaseType *other_field_ty = other_field->get_field_type ();

		TyTy::BaseType *unified_ty
		  = UnifyRules::Resolve (TyTy::TyWithLocation (this_field_ty),
					 TyTy::TyWithLocation (other_field_ty),
					 locus, commit_flag,
					 false /* emit_error */);
		if (unified_ty->get_kind () == TyTy::TypeKind::ERROR)
		  {
		    return new TyTy::ErrorType (0);
		  }
	      }
	  }

	// generic args for the unit-struct case
	if (type.is_unit () && ltype->is_unit ())
	  {
	    rust_assert (type.get_num_substitutions ()
			 == ltype->get_num_substitutions ());

	    for (size_t i = 0; i < type.get_num_substitutions (); i++)
	      {
		auto &a = ltype->get_substs ().at (i);
		auto &b = type.get_substs ().at (i);

		auto pa = a.get_param_ty ();
		auto pb = b.get_param_ty ();

		auto res
		  = UnifyRules::Resolve (TyTy::TyWithLocation (pa),
					 TyTy::TyWithLocation (pb), locus,
					 commit_flag, false /* emit_error */);
		if (res->get_kind () == TyTy::TypeKind::ERROR)
		  {
		    return new TyTy::ErrorType (0);
		  }
	      }
	  }

	return type.clone ();
      }
      break;

    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_str (TyTy::StrType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

    case TyTy::STR:
      return rtype->clone ();

    case TyTy::ADT:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_reference (TyTy::ReferenceType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::REF: {
	TyTy::ReferenceType &type = *static_cast<TyTy::ReferenceType *> (rtype);
	auto base_type = ltype->get_base ();
	auto other_base_type = type.get_base ();

	TyTy::BaseType *base_resolved
	  = UnifyRules::Resolve (TyTy::TyWithLocation (base_type),
				 TyTy::TyWithLocation (other_base_type), locus,
				 commit_flag, false /* emit_error */);
	if (base_resolved->get_kind () == TyTy::TypeKind::ERROR)
	  {
	    return new TyTy::ErrorType (0);
	  }

	// rust is permissive about mutablity here you can always go from
	// mutable to immutable but not the otherway round
	bool mutability_ok = ltype->is_mutable () ? type.is_mutable () : true;
	if (!mutability_ok)
	  {
	    return new TyTy::ErrorType (0);
	  }

	return new TyTy::ReferenceType (ltype->get_ref (), ltype->get_ty_ref (),
					TyTy::TyVar (base_resolved->get_ref ()),
					ltype->mutability ());
      }
      break;

    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_pointer (TyTy::PointerType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::POINTER: {
	TyTy::PointerType &type = *static_cast<TyTy::PointerType *> (rtype);
	auto base_type = ltype->get_base ();
	auto other_base_type = type.get_base ();

	TyTy::BaseType *base_resolved
	  = UnifyRules::Resolve (TyTy::TyWithLocation (base_type),
				 TyTy::TyWithLocation (other_base_type), locus,
				 commit_flag, false /* emit_error */);
	if (base_resolved->get_kind () == TyTy::TypeKind::ERROR)
	  {
	    return new TyTy::ErrorType (0);
	  }

	// rust is permissive about mutablity here you can always go from
	// mutable to immutable but not the otherway round
	bool mutability_ok = ltype->is_mutable () ? type.is_mutable () : true;
	if (!mutability_ok)
	  {
	    return new TyTy::ErrorType (0);
	  }

	return new TyTy::PointerType (ltype->get_ref (), ltype->get_ty_ref (),
				      TyTy::TyVar (base_resolved->get_ref ()),
				      ltype->mutability ());
      }
      break;

    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::REF:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_param (TyTy::ParamType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::PARAM: {
	TyTy::ParamType &type = *static_cast<TyTy::ParamType *> (rtype);
	// bool symbol_matches
	//   = ltype->get_symbol ().compare (type.get_symbol ()) == 0;
	// // TODO
	// // I think rustc checks a debruinj index
	// if (symbol_matches)
	//   {
	//     return type.clone ();
	//   }

	// matching symbol is not going to work when we mix symbol's and have
	// nested generics

	// bounds match? FIXME

	return type.clone ();
      }
      break;

    case TyTy::POINTER:
    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::REF:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_array (TyTy::ArrayType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::ARRAY: {
	TyTy::ArrayType &type = *static_cast<TyTy::ArrayType *> (rtype);
	TyTy::BaseType *element_unify = UnifyRules::Resolve (
	  TyTy::TyWithLocation (ltype->get_element_type ()),
	  TyTy::TyWithLocation (type.get_element_type ()), locus, commit_flag,
	  false /* emit_error*/);

	if (element_unify->get_kind () != TyTy::TypeKind::ERROR)
	  {
	    return new TyTy::ArrayType (type.get_ref (), type.get_ty_ref (),
					type.get_ident ().locus,
					type.get_capacity_expr (),
					TyTy::TyVar (
					  element_unify->get_ref ()));
	  }
      }
      break;

    case TyTy::PARAM:
    case TyTy::POINTER:
    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::REF:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_slice (TyTy::SliceType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::SLICE: {
	TyTy::SliceType &type = *static_cast<TyTy::SliceType *> (rtype);
	TyTy::BaseType *element_unify = UnifyRules::Resolve (
	  TyTy::TyWithLocation (ltype->get_element_type ()),
	  TyTy::TyWithLocation (type.get_element_type ()), locus, commit_flag,
	  false /* emit_error*/);

	if (element_unify->get_kind () != TyTy::TypeKind::ERROR)
	  {
	    return new TyTy::SliceType (type.get_ref (), type.get_ty_ref (),
					type.get_ident ().locus,
					TyTy::TyVar (
					  element_unify->get_ref ()));
	  }
      }
      break;

    case TyTy::PARAM:
    case TyTy::POINTER:
    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::REF:
    case TyTy::ARRAY:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_fndef (TyTy::FnType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::FNDEF: {
	TyTy::FnType &type = *static_cast<TyTy::FnType *> (rtype);
	if (ltype->num_params () != type.num_params ())
	  {
	    return new TyTy::ErrorType (0);
	  }

	for (size_t i = 0; i < ltype->num_params (); i++)
	  {
	    auto a = ltype->param_at (i).second;
	    auto b = type.param_at (i).second;

	    auto unified_param
	      = UnifyRules::Resolve (TyTy::TyWithLocation (a),
				     TyTy::TyWithLocation (b), locus,
				     commit_flag, false /* emit_errors */);
	    if (unified_param->get_kind () == TyTy::TypeKind::ERROR)
	      {
		return new TyTy::ErrorType (0);
	      }
	  }

	auto unified_return
	  = UnifyRules::Resolve (TyTy::TyWithLocation (
				   ltype->get_return_type ()),
				 TyTy::TyWithLocation (type.get_return_type ()),
				 locus, commit_flag, false /* emit_errors */);
	if (unified_return->get_kind () == TyTy::TypeKind::ERROR)
	  {
	    return new TyTy::ErrorType (0);
	  }

	return ltype->clone ();
      }
      break;

    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::FLOAT:
    case TyTy::ISIZE:
    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNPTR:
    case TyTy::UINT:
    case TyTy::USIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_fnptr (TyTy::FnPtr *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::FNPTR: {
	TyTy::FnPtr &type = *static_cast<TyTy::FnPtr *> (rtype);
	if (ltype->num_params () != type.num_params ())
	  {
	    return new TyTy::ErrorType (0);
	  }

	for (size_t i = 0; i < ltype->num_params (); i++)
	  {
	    auto a = ltype->param_at (i);
	    auto b = type.param_at (i);

	    auto unified_param
	      = UnifyRules::Resolve (TyTy::TyWithLocation (a),
				     TyTy::TyWithLocation (b), locus,
				     commit_flag, false /* emit_errors */);
	    if (unified_param->get_kind () == TyTy::TypeKind::ERROR)
	      {
		return new TyTy::ErrorType (0);
	      }
	  }

	auto unified_return
	  = UnifyRules::Resolve (TyTy::TyWithLocation (
				   ltype->get_return_type ()),
				 TyTy::TyWithLocation (type.get_return_type ()),
				 locus, commit_flag, false /* emit_errors */);
	if (unified_return->get_kind () == TyTy::TypeKind::ERROR)
	  {
	    return new TyTy::ErrorType (0);
	  }

	return ltype->clone ();
      }
      break;

      case TyTy::FNDEF: {
	TyTy::FnType &type = *static_cast<TyTy::FnType *> (rtype);
	auto this_ret_type = ltype->get_return_type ();
	auto other_ret_type = type.get_return_type ();

	auto unified_result
	  = UnifyRules::Resolve (TyTy::TyWithLocation (this_ret_type),
				 TyTy::TyWithLocation (other_ret_type), locus,
				 commit_flag, false /*emit_errors*/);
	if (unified_result->get_kind () == TyTy::TypeKind::ERROR)
	  {
	    return new TyTy::ErrorType (0);
	  }

	if (ltype->num_params () != type.num_params ())
	  {
	    return new TyTy::ErrorType (0);
	  }

	for (size_t i = 0; i < ltype->num_params (); i++)
	  {
	    auto this_param = ltype->param_at (i);
	    auto other_param = type.param_at (i).second;

	    auto unified_param
	      = UnifyRules::Resolve (TyTy::TyWithLocation (this_param),
				     TyTy::TyWithLocation (other_param), locus,
				     commit_flag, false /* emit_errors */);
	    if (unified_param->get_kind () == TyTy::TypeKind::ERROR)
	      {
		return new TyTy::ErrorType (0);
	      }
	  }

	return ltype->clone ();
      }
      break;

    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::FLOAT:
    case TyTy::ISIZE:
    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::UINT:
    case TyTy::USIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_tuple (TyTy::TupleType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::TUPLE: {
	TyTy::TupleType &type = *static_cast<TyTy::TupleType *> (rtype);
	if (ltype->num_fields () != type.num_fields ())
	  {
	    return new TyTy::ErrorType (0);
	  }

	std::vector<TyTy::TyVar> fields;
	for (size_t i = 0; i < ltype->num_fields (); i++)
	  {
	    TyTy::BaseType *bo = ltype->get_field (i);
	    TyTy::BaseType *fo = type.get_field (i);

	    TyTy::BaseType *unified_ty
	      = UnifyRules::Resolve (TyTy::TyWithLocation (bo),
				     TyTy::TyWithLocation (fo), locus,
				     commit_flag, false /* emit_errors */);
	    if (unified_ty->get_kind () == TyTy::TypeKind::ERROR)
	      return new TyTy::ErrorType (0);

	    fields.push_back (TyTy::TyVar (unified_ty->get_ref ()));
	  }

	return new TyTy::TupleType (type.get_ref (), type.get_ty_ref (),
				    type.get_ident ().locus, fields);
      }
      break;

    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::FLOAT:
    case TyTy::ISIZE:
    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::UINT:
    case TyTy::USIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_bool (TyTy::BoolType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

    case TyTy::BOOL:
      return rtype->clone ();

    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::FLOAT:
    case TyTy::ISIZE:
    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::UINT:
    case TyTy::USIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_char (TyTy::CharType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

    case TyTy::CHAR:
      return rtype->clone ();

    case TyTy::INT:
    case TyTy::FLOAT:
    case TyTy::ISIZE:
    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::UINT:
    case TyTy::USIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_int (TyTy::IntType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL
	    || r->get_infer_kind () == TyTy::InferType::InferTypeKind::INTEGRAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::INT: {
	TyTy::IntType &type = *static_cast<TyTy::IntType *> (rtype);
	bool is_valid = ltype->get_int_kind () == type.get_int_kind ();
	if (is_valid)
	  return new TyTy::IntType (type.get_ref (), type.get_ty_ref (),
				    type.get_int_kind ());
      }
      break;

    case TyTy::FLOAT:
    case TyTy::ISIZE:
    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::UINT:
    case TyTy::USIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_uint (TyTy::UintType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL
	    || r->get_infer_kind () == TyTy::InferType::InferTypeKind::INTEGRAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::UINT: {
	TyTy::UintType &type = *static_cast<TyTy::UintType *> (rtype);
	bool is_valid = ltype->get_uint_kind () == type.get_uint_kind ();
	if (is_valid)
	  return new TyTy::UintType (type.get_ref (), type.get_ty_ref (),
				     type.get_uint_kind ());
      }
      break;

    case TyTy::FLOAT:
    case TyTy::ISIZE:
    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::USIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_float (TyTy::FloatType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL
	    || r->get_infer_kind () == TyTy::InferType::InferTypeKind::FLOAT;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::FLOAT: {
	TyTy::FloatType &type = *static_cast<TyTy::FloatType *> (rtype);
	bool is_valid = ltype->get_float_kind () == type.get_float_kind ();
	if (is_valid)
	  return new TyTy::FloatType (type.get_ref (), type.get_ty_ref (),
				      type.get_float_kind ());
      }
      break;

    case TyTy::ISIZE:
    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::USIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_isize (TyTy::ISizeType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () != TyTy::InferType::InferTypeKind::FLOAT;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

    case TyTy::ISIZE:
      return rtype->clone ();

    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_usize (TyTy::USizeType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () != TyTy::InferType::InferTypeKind::FLOAT;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

    case TyTy::USIZE:
      return rtype->clone ();

    case TyTy::ADT:
    case TyTy::STR:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::ARRAY:
    case TyTy::SLICE:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_never (TyTy::NeverType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

    case TyTy::NEVER:
      return rtype->clone ();

    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::SLICE:
    case TyTy::PARAM:
    case TyTy::POINTER:
    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::REF:
    case TyTy::ARRAY:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_placeholder (TyTy::PlaceholderType *ltype,
				TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::PLACEHOLDER: {
	TyTy::PlaceholderType &type
	  = *static_cast<TyTy::PlaceholderType *> (rtype);
	bool symbol_match
	  = ltype->get_symbol ().compare (type.get_symbol ()) == 0;
	if (symbol_match)
	  {
	    return type.clone ();
	  }
      }
      break;

    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::SLICE:
    case TyTy::PARAM:
    case TyTy::POINTER:
    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::REF:
    case TyTy::ARRAY:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_projection (TyTy::ProjectionType *ltype,
			       TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      // FIXME
    case TyTy::PROJECTION:
      gcc_unreachable ();
      break;

    case TyTy::DYNAMIC:
    case TyTy::CLOSURE:
    case TyTy::SLICE:
    case TyTy::PARAM:
    case TyTy::POINTER:
    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::REF:
    case TyTy::ARRAY:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_dyn (TyTy::DynamicObjectType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::DYNAMIC: {
	TyTy::DynamicObjectType &type
	  = *static_cast<TyTy::DynamicObjectType *> (rtype);
	if (ltype->num_specified_bounds () != type.num_specified_bounds ())
	  {
	    return new TyTy::ErrorType (0);
	  }

	if (!ltype->bounds_compatible (type, locus, true))
	  {
	    return new TyTy::ErrorType (0);
	  }

	return ltype->clone ();
      }
      break;

    case TyTy::CLOSURE:
    case TyTy::SLICE:
    case TyTy::PARAM:
    case TyTy::POINTER:
    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::REF:
    case TyTy::ARRAY:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

TyTy::BaseType *
UnifyRules::expect_closure (TyTy::ClosureType *ltype, TyTy::BaseType *rtype)
{
  switch (rtype->get_kind ())
    {
      case TyTy::INFER: {
	TyTy::InferType *r = static_cast<TyTy::InferType *> (rtype);
	bool is_valid
	  = r->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL;
	if (is_valid)
	  return ltype->clone ();
      }
      break;

      case TyTy::CLOSURE: {
	TyTy::ClosureType &type = *static_cast<TyTy::ClosureType *> (rtype);
	if (ltype->get_def_id () != type.get_def_id ())
	  {
	    return new TyTy::ErrorType (0);
	  }

	TyTy::BaseType *args_res
	  = UnifyRules::Resolve (TyTy::TyWithLocation (
				   &ltype->get_parameters ()),
				 TyTy::TyWithLocation (&type.get_parameters ()),
				 locus, commit_flag, false /* emit_error */);
	if (args_res->get_kind () == TyTy::TypeKind::ERROR)
	  {
	    return new TyTy::ErrorType (0);
	  }

	TyTy::BaseType *res = UnifyRules::Resolve (
	  TyTy::TyWithLocation (&ltype->get_result_type ()),
	  TyTy::TyWithLocation (&type.get_result_type ()), locus, commit_flag,
	  false /* emit_error */);
	if (res == nullptr || res->get_kind () == TyTy::TypeKind::ERROR)
	  {
	    return new TyTy::ErrorType (0);
	  }

	return ltype->clone ();
      }
      break;

    case TyTy::SLICE:
    case TyTy::PARAM:
    case TyTy::POINTER:
    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::REF:
    case TyTy::ARRAY:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::TUPLE:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::DYNAMIC:
    case TyTy::ERROR:
      return new TyTy::ErrorType (0);
    }
  return new TyTy::ErrorType (0);
}

} // namespace Resolver
} // namespace Rust
