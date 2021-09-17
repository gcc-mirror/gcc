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

#ifndef RUST_TYTY_COERCION_RULES
#define RUST_TYTY_COERCION_RULES

#include "rust-diagnostics.h"
#include "rust-tyty.h"
#include "rust-tyty-visitor.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check.h"

extern ::Backend *
rust_get_backend ();

namespace Rust {
namespace TyTy {

class BaseCoercionRules : public TyVisitor
{
public:
  virtual ~BaseCoercionRules () {}

  virtual BaseType *coerce (BaseType *other)
  {
    if (other->get_kind () == TypeKind::PARAM)
      {
	ParamType *p = static_cast<ParamType *> (other);
	if (p->can_resolve ())
	  {
	    other = p->resolve ();
	  }
      }
    else if (other->get_kind () == TypeKind::PLACEHOLDER)
      {
	PlaceholderType *p = static_cast<PlaceholderType *> (other);
	if (p->can_resolve ())
	  {
	    other = p->resolve ();
	  }
      }
    else if (other->get_kind () == TypeKind::PROJECTION)
      {
	ProjectionType *p = static_cast<ProjectionType *> (other);
	other = p->get ();
      }

    other->accept_vis (*this);
    if (resolved->get_kind () == TyTy::TypeKind::ERROR)
      return resolved;

    resolved->append_reference (get_base ()->get_ref ());
    resolved->append_reference (other->get_ref ());
    for (auto ref : get_base ()->get_combined_refs ())
      resolved->append_reference (ref);
    for (auto ref : other->get_combined_refs ())
      resolved->append_reference (ref);

    bool result_resolved = resolved->get_kind () != TyTy::TypeKind::INFER;
    bool result_is_infer_var = resolved->get_kind () == TyTy::TypeKind::INFER;
    bool results_is_non_general_infer_var
      = (result_is_infer_var
	 && (static_cast<InferType *> (resolved))->get_infer_kind ()
	      != TyTy::InferType::GENERAL);
    if (result_resolved || results_is_non_general_infer_var)
      {
	for (auto &ref : resolved->get_combined_refs ())
	  {
	    TyTy::BaseType *ref_tyty = nullptr;
	    bool ok = context->lookup_type (ref, &ref_tyty);
	    if (!ok)
	      continue;

	    // if any of the types are inference variables lets fix them
	    if (ref_tyty->get_kind () == TyTy::TypeKind::INFER)
	      {
		context->insert_type (
		  Analysis::NodeMapping (mappings->get_current_crate (),
					 UNKNOWN_NODEID, ref,
					 UNKNOWN_LOCAL_DEFID),
		  resolved->clone ());
	      }
	  }
      }
    return resolved;
  }

  virtual void visit (TupleType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (ADTType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (InferType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (FnType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (FnPtr &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (ArrayType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (BoolType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (IntType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (UintType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (USizeType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (ISizeType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (FloatType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (ErrorType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (CharType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (ReferenceType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (PointerType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (ParamType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (StrType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (NeverType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (PlaceholderType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (ProjectionType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

  virtual void visit (DynamicObjectType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    Location base_locus = mappings->lookup_location (get_base ()->get_ref ());
    RichLocation r (ref_locus);
    r.add_range (base_locus);
    rust_error_at (r, "expected [%s] got [%s]",
		   get_base ()->as_string ().c_str (),
		   type.as_string ().c_str ());
  }

protected:
  BaseCoercionRules (BaseType *base)
    : mappings (Analysis::Mappings::get ()),
      context (Resolver::TypeCheckContext::get ()),
      resolved (new ErrorType (base->get_ref (), base->get_ref ()))
  {}

  Analysis::Mappings *mappings;
  Resolver::TypeCheckContext *context;

  /* Temporary storage for the result of a unification.
     We could return the result directly instead of storing it in the rule
     object, but that involves modifying the visitor pattern to accommodate
     the return value, which is too complex. */
  BaseType *resolved;

private:
  /* Returns a pointer to the ty that created this rule. */
  virtual BaseType *get_base () = 0;
};

class InferCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  InferCoercionRules (InferType *base) : BaseCoercionRules (base), base (base)
  {}

  void visit (BoolType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (IntType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (UintType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (USizeType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (ISizeType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (FloatType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind () == TyTy::InferType::InferTypeKind::FLOAT);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (ArrayType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (ADTType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (TupleType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (InferType &type) override
  {
    switch (base->get_infer_kind ())
      {
      case InferType::InferTypeKind::GENERAL:
	resolved = type.clone ();
	return;

	case InferType::InferTypeKind::INTEGRAL: {
	  if (type.get_infer_kind () == InferType::InferTypeKind::INTEGRAL)
	    {
	      resolved = type.clone ();
	      return;
	    }
	  else if (type.get_infer_kind () == InferType::InferTypeKind::GENERAL)
	    {
	      resolved = base->clone ();
	      return;
	    }
	}
	break;

	case InferType::InferTypeKind::FLOAT: {
	  if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
	    {
	      resolved = type.clone ();
	      return;
	    }
	  else if (type.get_infer_kind () == InferType::InferTypeKind::GENERAL)
	    {
	      resolved = base->clone ();
	      return;
	    }
	}
	break;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (CharType &type) override
  {
    {
      bool is_valid
	= (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
      if (is_valid)
	{
	  resolved = type.clone ();
	  return;
	}

      BaseCoercionRules::visit (type);
    }
  }

  void visit (ReferenceType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (PointerType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (ParamType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (DynamicObjectType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseCoercionRules::visit (type);
  }

private:
  BaseType *get_base () override { return base; }

  InferType *base;
};

class FnCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  FnCoercionRules (FnType *base) : BaseCoercionRules (base), base (base) {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (FnType &type) override
  {
    if (base->num_params () != type.num_params ())
      {
	BaseCoercionRules::visit (type);
	return;
      }

    for (size_t i = 0; i < base->num_params (); i++)
      {
	auto a = base->param_at (i).second;
	auto b = type.param_at (i).second;

	auto unified_param = a->unify (b);
	if (unified_param == nullptr)
	  {
	    BaseCoercionRules::visit (type);
	    return;
	  }
      }

    auto unified_return
      = base->get_return_type ()->unify (type.get_return_type ());
    if (unified_return == nullptr)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  BaseType *get_base () override { return base; }

  FnType *base;
};

class FnptrCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  FnptrCoercionRules (FnPtr *base) : BaseCoercionRules (base), base (base) {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (FnPtr &type) override
  {
    auto this_ret_type = base->get_return_type ();
    auto other_ret_type = type.get_return_type ();
    auto unified_result = this_ret_type->unify (other_ret_type);
    if (unified_result == nullptr
	|| unified_result->get_kind () == TypeKind::ERROR)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    if (base->num_params () != type.num_params ())
      {
	BaseCoercionRules::visit (type);
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
	    BaseCoercionRules::visit (type);
	    return;
	  }
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (FnType &type) override
  {
    auto this_ret_type = base->get_return_type ();
    auto other_ret_type = type.get_return_type ();
    auto unified_result = this_ret_type->unify (other_ret_type);
    if (unified_result == nullptr
	|| unified_result->get_kind () == TypeKind::ERROR)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    if (base->num_params () != type.num_params ())
      {
	BaseCoercionRules::visit (type);
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
	    BaseCoercionRules::visit (type);
	    return;
	  }
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  BaseType *get_base () override { return base; }

  FnPtr *base;
};

class ArrayCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  ArrayCoercionRules (ArrayType *base) : BaseCoercionRules (base), base (base)
  {}

  void visit (ArrayType &type) override
  {
    // check base type
    auto base_resolved
      = base->get_element_type ()->unify (type.get_element_type ());
    if (base_resolved == nullptr)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    auto backend = rust_get_backend ();

    // need to check the base types and capacity
    if (!backend->const_values_equal (type.get_capacity (),
				      base->get_capacity ()))
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved
      = new ArrayType (type.get_ref (), type.get_ty_ref (),
		       type.get_capacity (), TyVar (base_resolved->get_ref ()));
  }

private:
  BaseType *get_base () override { return base; }

  ArrayType *base;
};

class BoolCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  BoolCoercionRules (BoolType *base) : BaseCoercionRules (base), base (base) {}

  void visit (BoolType &type) override
  {
    resolved = new BoolType (type.get_ref (), type.get_ty_ref ());
  }

  void visit (InferType &type) override
  {
    switch (type.get_infer_kind ())
      {
      case InferType::InferTypeKind::GENERAL:
	resolved = base->clone ();
	break;

      default:
	BaseCoercionRules::visit (type);
	break;
      }
  }

private:
  BaseType *get_base () override { return base; }

  BoolType *base;
};

class IntCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  IntCoercionRules (IntType *base) : BaseCoercionRules (base), base (base) {}

  void visit (InferType &type) override
  {
    // cant assign a float inference variable
    if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (IntType &type) override
  {
    if (type.get_int_kind () != base->get_int_kind ())
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved
      = new IntType (type.get_ref (), type.get_ty_ref (), type.get_int_kind ());
  }

private:
  BaseType *get_base () override { return base; }

  IntType *base;
};

class UintCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  UintCoercionRules (UintType *base) : BaseCoercionRules (base), base (base) {}

  void visit (InferType &type) override
  {
    // cant assign a float inference variable
    if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (UintType &type) override
  {
    if (type.get_uint_kind () != base->get_uint_kind ())
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = new UintType (type.get_ref (), type.get_ty_ref (),
			     type.get_uint_kind ());
  }

private:
  BaseType *get_base () override { return base; }

  UintType *base;
};

class FloatCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  FloatCoercionRules (FloatType *base) : BaseCoercionRules (base), base (base)
  {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () == InferType::InferTypeKind::INTEGRAL)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (FloatType &type) override
  {
    if (type.get_float_kind () != base->get_float_kind ())
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = new FloatType (type.get_ref (), type.get_ty_ref (),
			      type.get_float_kind ());
  }

private:
  BaseType *get_base () override { return base; }

  FloatType *base;
};

class ADTCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  ADTCoercionRules (ADTType *base) : BaseCoercionRules (base), base (base) {}

  void visit (ADTType &type) override
  {
    if (base->get_identifier ().compare (type.get_identifier ()) != 0)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    if (base->num_fields () != type.num_fields ())
      {
	BaseCoercionRules::visit (type);
	return;
      }

    for (size_t i = 0; i < type.num_fields (); ++i)
      {
	TyTy::StructFieldType *base_field = base->get_field (i);
	TyTy::StructFieldType *other_field = type.get_field (i);

	TyTy::BaseType *this_field_ty = base_field->get_field_type ();
	TyTy::BaseType *other_field_ty = other_field->get_field_type ();

	BaseType *unified_ty = this_field_ty->unify (other_field_ty);
	if (unified_ty->get_kind () == TyTy::TypeKind::ERROR)
	  return;
      }

    resolved = type.clone ();
  }

private:
  BaseType *get_base () override { return base; }

  ADTType *base;
};

class TupleCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  TupleCoercionRules (TupleType *base) : BaseCoercionRules (base), base (base)
  {}

  void visit (TupleType &type) override
  {
    if (base->num_fields () != type.num_fields ())
      {
	BaseCoercionRules::visit (type);
	return;
      }

    std::vector<TyVar> fields;
    for (size_t i = 0; i < base->num_fields (); i++)
      {
	BaseType *bo = base->get_field (i);
	BaseType *fo = type.get_field (i);

	BaseType *unified_ty = bo->unify (fo);
	if (unified_ty->get_kind () == TyTy::TypeKind::ERROR)
	  return;

	fields.push_back (TyVar (unified_ty->get_ref ()));
      }

    resolved
      = new TyTy::TupleType (type.get_ref (), type.get_ty_ref (), fields);
  }

private:
  BaseType *get_base () override { return base; }

  TupleType *base;
};

class USizeCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  USizeCoercionRules (USizeType *base) : BaseCoercionRules (base), base (base)
  {}

  void visit (InferType &type) override
  {
    // cant assign a float inference variable
    if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (USizeType &type) override { resolved = type.clone (); }

private:
  BaseType *get_base () override { return base; }

  USizeType *base;
};

class ISizeCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  ISizeCoercionRules (ISizeType *base) : BaseCoercionRules (base), base (base)
  {}

  void visit (InferType &type) override
  {
    // cant assign a float inference variable
    if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (ISizeType &type) override { resolved = type.clone (); }

private:
  BaseType *get_base () override { return base; }

  ISizeType *base;
};

class CharCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  CharCoercionRules (CharType *base) : BaseCoercionRules (base), base (base) {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (CharType &type) override { resolved = type.clone (); }

private:
  BaseType *get_base () override { return base; }

  CharType *base;
};

class ReferenceCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  ReferenceCoercionRules (ReferenceType *base)
    : BaseCoercionRules (base), base (base)
  {}

  void visit (ReferenceType &type) override
  {
    auto base_type = base->get_base ();
    auto other_base_type = type.get_base ();

    TyTy::BaseType *base_resolved = base_type->coerce (other_base_type);
    if (base_resolved == nullptr
	|| base_resolved->get_kind () == TypeKind::ERROR)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    // we can allow for mutability changes here by casting down from mutability
    // eg:  mut vs const, we cant take a mutable reference from a const
    // eg:  const vs mut we can take a const reference from a mutable one
    if (!base->is_mutable () || (base->is_mutable () == type.is_mutable ()))
      {
	resolved = new ReferenceType (base->get_ref (), base->get_ty_ref (),
				      TyVar (base_resolved->get_ref ()),
				      base->is_mutable ());
	return;
      }

    BaseCoercionRules::visit (type);
  }

private:
  BaseType *get_base () override { return base; }

  ReferenceType *base;
};

class PointerCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  PointerCoercionRules (PointerType *base)
    : BaseCoercionRules (base), base (base)
  {}

  void visit (ReferenceType &type) override
  {
    auto base_type = base->get_base ();
    auto other_base_type = type.get_base ();

    TyTy::BaseType *base_resolved = base_type->unify (other_base_type);
    if (base_resolved == nullptr
	|| base_resolved->get_kind () == TypeKind::ERROR)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    // we can allow for mutability changes here by casting down from mutability
    // eg:  mut vs const, we cant take a mutable pointer from a const
    // eg:  const vs mut we can take a const reference from a mutable one
    if (!base->is_mutable () || (base->is_mutable () == type.is_mutable ()))
      {
	resolved = new PointerType (base->get_ref (), base->get_ty_ref (),
				    TyVar (base_resolved->get_ref ()),
				    base->is_mutable ());
	return;
      }

    BaseCoercionRules::visit (type);
  }

  void visit (PointerType &type) override
  {
    auto base_type = base->get_base ();
    auto other_base_type = type.get_base ();

    TyTy::BaseType *base_resolved = base_type->unify (other_base_type);
    if (base_resolved == nullptr
	|| base_resolved->get_kind () == TypeKind::ERROR)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    // we can allow for mutability changes here by casting down from mutability
    // eg:  mut vs const, we cant take a mutable pointer from a const one
    // eg:  const vs mut we can take a const reference from a mutable one
    if (!base->is_mutable () || (base->is_mutable () == type.is_mutable ()))
      {
	resolved = new PointerType (base->get_ref (), base->get_ty_ref (),
				    TyVar (base_resolved->get_ref ()),
				    base->is_mutable ());
	return;
      }

    BaseCoercionRules::visit (type);
  }

private:
  BaseType *get_base () override { return base; }

  PointerType *base;
};

class ParamCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  ParamCoercionRules (ParamType *base) : BaseCoercionRules (base), base (base)
  {}

  // param types are a placeholder we shouldn't have cases where we unify
  // against it. eg: struct foo<T> { a: T }; When we invoke it we can do either:
  //
  // foo<i32>{ a: 123 }.
  // Then this enforces the i32 type to be referenced on the
  // field via an hirid.
  //
  // rust also allows for a = foo{a:123}; Where we can use an Inference Variable
  // to handle the typing of the struct
  BaseType *coerce (BaseType *other) override final
  {
    if (base->get_ref () == base->get_ty_ref ())
      return BaseCoercionRules::coerce (other);

    auto context = Resolver::TypeCheckContext::get ();
    BaseType *lookup = nullptr;
    bool ok = context->lookup_type (base->get_ty_ref (), &lookup);
    rust_assert (ok);

    return lookup->unify (other);
  }

  void visit (ParamType &type) override
  {
    if (base->get_symbol ().compare (type.get_symbol ()) != 0)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = type.clone ();
  }

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
  }

private:
  BaseType *get_base () override { return base; }

  ParamType *base;
};

class StrCoercionRules : public BaseCoercionRules
{
  // FIXME we will need a enum for the StrType like ByteBuf etc..
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  StrCoercionRules (StrType *base) : BaseCoercionRules (base), base (base) {}

  void visit (StrType &type) override { resolved = type.clone (); }

private:
  BaseType *get_base () override { return base; }

  StrType *base;
};

class NeverCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  NeverCoercionRules (NeverType *base) : BaseCoercionRules (base), base (base)
  {}

  virtual void visit (NeverType &type) override { resolved = type.clone (); }

private:
  BaseType *get_base () override { return base; }

  NeverType *base;
};

class PlaceholderCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  PlaceholderCoercionRules (PlaceholderType *base)
    : BaseCoercionRules (base), base (base)
  {}

  BaseType *coerce (BaseType *other) override final
  {
    if (!base->can_resolve ())
      return BaseCoercionRules::coerce (other);

    BaseType *lookup = base->resolve ();
    return lookup->unify (other);
  }

  void visit (PlaceholderType &type) override
  {
    if (base->get_symbol ().compare (type.get_symbol ()) != 0)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = type.clone ();
  }

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
  }

private:
  BaseType *get_base () override { return base; }

  PlaceholderType *base;
};

class DynamicCoercionRules : public BaseCoercionRules
{
  using Rust::TyTy::BaseCoercionRules::visit;

public:
  DynamicCoercionRules (DynamicObjectType *base)
    : BaseCoercionRules (base), base (base)
  {}

  void visit (DynamicObjectType &type) override
  {
    if (base->num_specified_bounds () != type.num_specified_bounds ())
      {
	BaseCoercionRules::visit (type);
	return;
      }

    Location ref_locus = mappings->lookup_location (type.get_ref ());
    if (!base->bounds_compatible (type, ref_locus, true))
      {
	BaseCoercionRules::visit (type);
	return;
      }

    resolved = base->clone ();
  }

private:
  BaseType *get_base () override { return base; }

  DynamicObjectType *base;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_COERCION_RULES
