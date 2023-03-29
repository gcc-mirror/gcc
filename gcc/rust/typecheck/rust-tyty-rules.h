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

#ifndef RUST_TYTY_RULES
#define RUST_TYTY_RULES

#include "rust-diagnostics.h"
#include "rust-tyty.h"
#include "rust-tyty-visitor.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace TyTy {

/* Rules specify how to unify two Ty. For example, the result of unifying the
   two tuples (u64, A) and (B, i64) would be (u64, i64).

   Performing a unification requires a double dispatch. To illustrate, suppose
   we want to unify `ty1` and `ty2`. Here's what it looks like:
     1. The caller calls `ty1.unify(ty2)`. This is the first dispatch.
     2. `ty1` creates a rule specific to its type(e.g. TupleRules).
     3. The rule calls `ty2.accept_vis(rule)`. This is the second dispatch.
     4. `ty2` calls `rule.visit(*this)`, which will method-overload to the
	      correct implementation at compile time.

   The nice thing about Rules is that they seperate unification logic from the
   representation of Ty. To support unifying a new Ty, implement its
   `accept_vis` and `unify` method to pass the unification request to Rules.
   Then, create a new `XXXRules` class and implement one `visit` method for
   every Ty it can unify with. */
class BaseRules : public TyVisitor
{
public:
  virtual ~BaseRules () {}

  /* Unify two ty. Returns a pointer to the newly-created unified ty, or nullptr
     if the two types cannot be unified. The caller is responsible for releasing
     the memory of the returned ty.

     This method is meant to be used internally by Ty. If you're trying to unify
     two ty, you can simply call `unify` on ty themselves. */
  virtual BaseType *unify (BaseType *other)
  {
    if (other->get_kind () == TypeKind::PARAM)
      {
	ParamType *p = static_cast<ParamType *> (other);
	other = p->resolve ();
      }
    else if (other->get_kind () == TypeKind::PLACEHOLDER)
      {
	PlaceholderType *p = static_cast<PlaceholderType *> (other);
	if (p->can_resolve ())
	  {
	    other = p->resolve ();
	    return get_base ()->unify (other);
	  }
      }
    else if (other->get_kind () == TypeKind::PROJECTION)
      {
	ProjectionType *p = static_cast<ProjectionType *> (other);
	other = p->get ();
	return get_base ()->unify (other);
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

    other->append_reference (resolved->get_ref ());
    other->append_reference (get_base ()->get_ref ());
    get_base ()->append_reference (resolved->get_ref ());
    get_base ()->append_reference (other->get_ref ());

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

  virtual void visit (TupleType &) override {}

  virtual void visit (ADTType &) override {}

  virtual void visit (InferType &) override {}

  virtual void visit (FnType &) override {}

  virtual void visit (FnPtr &) override {}

  virtual void visit (ArrayType &) override {}

  virtual void visit (SliceType &) override {}

  virtual void visit (BoolType &) override {}

  virtual void visit (IntType &) override {}

  virtual void visit (UintType &) override {}

  virtual void visit (USizeType &) override {}

  virtual void visit (ISizeType &) override {}

  virtual void visit (FloatType &) override {}

  virtual void visit (ErrorType &) override {}

  virtual void visit (CharType &) override {}

  virtual void visit (ReferenceType &) override {}

  virtual void visit (PointerType &) override {}

  virtual void visit (ParamType &) override {}

  virtual void visit (StrType &) override {}

  virtual void visit (NeverType &) override {}

  virtual void visit (PlaceholderType &) override {}

  virtual void visit (ProjectionType &) override {}

  virtual void visit (DynamicObjectType &) override {}

  virtual void visit (ClosureType &) override {}

protected:
  BaseRules (BaseType *base)
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

class InferRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  InferRules (InferType *base) : BaseRules (base), base (base) {}

  void visit (BoolType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseRules::visit (type);
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

    BaseRules::visit (type);
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

    BaseRules::visit (type);
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

    BaseRules::visit (type);
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

    BaseRules::visit (type);
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

    BaseRules::visit (type);
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

    BaseRules::visit (type);
  }

  void visit (SliceType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseRules::visit (type);
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

    BaseRules::visit (type);
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

    BaseRules::visit (type);
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

    BaseRules::visit (type);
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

      BaseRules::visit (type);
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

    BaseRules::visit (type);
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

    BaseRules::visit (type);
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

    BaseRules::visit (type);
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

    BaseRules::visit (type);
  }

  void visit (ClosureType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	resolved = type.clone ();
	return;
      }

    BaseRules::visit (type);
  }

private:
  BaseType *get_base () override { return base; }

  InferType *base;
};

class FnRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  FnRules (FnType *base) : BaseRules (base), base (base) {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (FnType &type) override
  {
    if (base->num_params () != type.num_params ())
      {
	BaseRules::visit (type);
	return;
      }

    for (size_t i = 0; i < base->num_params (); i++)
      {
	auto a = base->param_at (i).second;
	auto b = type.param_at (i).second;

	auto unified_param = a->unify (b);
	if (unified_param == nullptr)
	  {
	    BaseRules::visit (type);
	    return;
	  }
      }

    auto unified_return
      = base->get_return_type ()->unify (type.get_return_type ());
    if (unified_return == nullptr)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  BaseType *get_base () override { return base; }

  FnType *base;
};

class FnptrRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  FnptrRules (FnPtr *base) : BaseRules (base), base (base) {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
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
	BaseRules::visit (type);
	return;
      }

    if (base->num_params () != type.num_params ())
      {
	BaseRules::visit (type);
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
	    BaseRules::visit (type);
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
	BaseRules::visit (type);
	return;
      }

    if (base->num_params () != type.num_params ())
      {
	BaseRules::visit (type);
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
	    BaseRules::visit (type);
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

class ClosureRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  ClosureRules (ClosureType *base) : BaseRules (base), base (base) {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (ClosureType &type) override
  {
    if (base->get_def_id () != type.get_def_id ())
      {
	BaseRules::visit (type);
	return;
      }

    TyTy::BaseType *args_res
      = base->get_parameters ().unify (&type.get_parameters ());
    if (args_res == nullptr || args_res->get_kind () == TypeKind::ERROR)
      {
	BaseRules::visit (type);
	return;
      }

    TyTy::BaseType *res
      = base->get_result_type ().unify (&type.get_result_type ());
    if (res == nullptr || res->get_kind () == TypeKind::ERROR)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  BaseType *get_base () override { return base; }

  ClosureType *base;
};

class ArrayRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  ArrayRules (ArrayType *base) : BaseRules (base), base (base) {}

  void visit (ArrayType &type) override
  {
    // check base type
    auto base_resolved
      = base->get_element_type ()->unify (type.get_element_type ());
    if (base_resolved == nullptr)
      {
	BaseRules::visit (type);
	return;
      }

    resolved
      = new ArrayType (type.get_ref (), type.get_ty_ref (),
		       type.get_ident ().locus, type.get_capacity_expr (),
		       TyVar (base_resolved->get_ref ()));
  }

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  BaseType *get_base () override { return base; }

  ArrayType *base;
};

class SliceRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  SliceRules (SliceType *base) : BaseRules (base), base (base) {}

  void visit (SliceType &type) override
  {
    // check base type
    auto base_resolved
      = base->get_element_type ()->unify (type.get_element_type ());
    if (base_resolved == nullptr)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = new SliceType (type.get_ref (), type.get_ty_ref (),
			      type.get_ident ().locus,
			      TyVar (base_resolved->get_ref ()));
  }

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  BaseType *get_base () override { return base; }

  SliceType *base;
};

class BoolRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  BoolRules (BoolType *base) : BaseRules (base), base (base) {}

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
	BaseRules::visit (type);
	break;
      }
  }

private:
  BaseType *get_base () override { return base; }

  BoolType *base;
};

class IntRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  IntRules (IntType *base) : BaseRules (base), base (base) {}

  void visit (InferType &type) override
  {
    // cant assign a float inference variable
    if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (IntType &type) override
  {
    if (type.get_int_kind () != base->get_int_kind ())
      {
	BaseRules::visit (type);
	return;
      }

    resolved
      = new IntType (type.get_ref (), type.get_ty_ref (), type.get_int_kind ());
  }

private:
  BaseType *get_base () override { return base; }

  IntType *base;
};

class UintRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  UintRules (UintType *base) : BaseRules (base), base (base) {}

  void visit (InferType &type) override
  {
    // cant assign a float inference variable
    if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (UintType &type) override
  {
    if (type.get_uint_kind () != base->get_uint_kind ())
      {
	BaseRules::visit (type);
	return;
      }

    resolved = new UintType (type.get_ref (), type.get_ty_ref (),
			     type.get_uint_kind ());
  }

private:
  BaseType *get_base () override { return base; }

  UintType *base;
};

class FloatRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  FloatRules (FloatType *base) : BaseRules (base), base (base) {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () == InferType::InferTypeKind::INTEGRAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

  void visit (FloatType &type) override
  {
    if (type.get_float_kind () != base->get_float_kind ())
      {
	BaseRules::visit (type);
	return;
      }

    resolved = new FloatType (type.get_ref (), type.get_ty_ref (),
			      type.get_float_kind ());
  }

private:
  BaseType *get_base () override { return base; }

  FloatType *base;
};

class ADTRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  ADTRules (ADTType *base) : BaseRules (base), base (base) {}

  void visit (ADTType &type) override
  {
    if (base->get_adt_kind () != type.get_adt_kind ())
      {
	BaseRules::visit (type);
	return;
      }

    if (base->get_identifier ().compare (type.get_identifier ()) != 0)
      {
	BaseRules::visit (type);
	return;
      }

    if (base->number_of_variants () != type.number_of_variants ())
      {
	BaseRules::visit (type);
	return;
      }

    for (size_t i = 0; i < type.number_of_variants (); ++i)
      {
	TyTy::VariantDef *a = base->get_variants ().at (i);
	TyTy::VariantDef *b = type.get_variants ().at (i);

	if (a->num_fields () != b->num_fields ())
	  {
	    BaseRules::visit (type);
	    return;
	  }

	for (size_t j = 0; j < a->num_fields (); j++)
	  {
	    TyTy::StructFieldType *base_field = a->get_field_at_index (j);
	    TyTy::StructFieldType *other_field = b->get_field_at_index (j);

	    TyTy::BaseType *this_field_ty = base_field->get_field_type ();
	    TyTy::BaseType *other_field_ty = other_field->get_field_type ();

	    BaseType *unified_ty = this_field_ty->unify (other_field_ty);
	    if (unified_ty->get_kind () == TyTy::TypeKind::ERROR)
	      return;
	  }
      }

    // generic args for the unit-struct case
    if (type.is_unit () && base->is_unit ())
      {
	rust_assert (type.get_num_substitutions ()
		     == base->get_num_substitutions ());

	for (size_t i = 0; i < type.get_num_substitutions (); i++)
	  {
	    auto &a = base->get_substs ().at (i);
	    auto &b = type.get_substs ().at (i);

	    auto pa = a.get_param_ty ();
	    auto pb = b.get_param_ty ();

	    auto res = pa->unify (pb);
	    if (res->get_kind () == TyTy::TypeKind::ERROR)
	      {
		return;
	      }
	  }
      }

    resolved = type.clone ();
  }

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  BaseType *get_base () override { return base; }

  ADTType *base;
};

class TupleRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  TupleRules (TupleType *base) : BaseRules (base), base (base) {}

  void visit (TupleType &type) override
  {
    if (base->num_fields () != type.num_fields ())
      {
	BaseRules::visit (type);
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

    resolved = new TyTy::TupleType (type.get_ref (), type.get_ty_ref (),
				    type.get_ident ().locus, fields);
  }

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  BaseType *get_base () override { return base; }

  TupleType *base;
};

class USizeRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  USizeRules (USizeType *base) : BaseRules (base), base (base) {}

  void visit (InferType &type) override
  {
    // cant assign a float inference variable
    if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
      {
	BaseRules::visit (type);
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

class ISizeRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  ISizeRules (ISizeType *base) : BaseRules (base), base (base) {}

  void visit (InferType &type) override
  {
    // cant assign a float inference variable
    if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
      {
	BaseRules::visit (type);
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

class CharRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  CharRules (CharType *base) : BaseRules (base), base (base) {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
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

class ReferenceRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  ReferenceRules (ReferenceType *base) : BaseRules (base), base (base) {}

  void visit (ReferenceType &type) override
  {
    auto base_type = base->get_base ();
    auto other_base_type = type.get_base ();

    TyTy::BaseType *base_resolved = base_type->unify (other_base_type);
    if (base_resolved == nullptr
	|| base_resolved->get_kind () == TypeKind::ERROR)
      {
	BaseRules::visit (type);
	return;
      }

    // rust is permissive about mutablity here you can always go from mutable to
    // immutable but not the otherway round
    bool mutability_ok = base->is_mutable () ? type.is_mutable () : true;
    if (!mutability_ok)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = new ReferenceType (base->get_ref (), base->get_ty_ref (),
				  TyVar (base_resolved->get_ref ()),
				  base->mutability ());
  }

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  BaseType *get_base () override { return base; }

  ReferenceType *base;
};

class PointerRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  PointerRules (PointerType *base) : BaseRules (base), base (base) {}

  void visit (PointerType &type) override
  {
    auto base_type = base->get_base ();
    auto other_base_type = type.get_base ();

    TyTy::BaseType *base_resolved = base_type->unify (other_base_type);
    if (base_resolved == nullptr
	|| base_resolved->get_kind () == TypeKind::ERROR)
      {
	BaseRules::visit (type);
	return;
      }

    // rust is permissive about mutablity here you can always go from mutable to
    // immutable but not the otherway round
    bool mutability_ok = base->is_mutable () ? type.is_mutable () : true;
    if (!mutability_ok)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = new PointerType (base->get_ref (), base->get_ty_ref (),
				TyVar (base_resolved->get_ref ()),
				base->mutability ());
  }

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  BaseType *get_base () override { return base; }

  PointerType *base;
};

class ParamRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  ParamRules (ParamType *base) : BaseRules (base), base (base) {}

  // param types are a placeholder we shouldn't have cases where we unify
  // against it. eg: struct foo<T> { a: T }; When we invoke it we can do either:
  //
  // foo<i32>{ a: 123 }.
  // Then this enforces the i32 type to be referenced on the
  // field via an hirid.
  //
  // rust also allows for a = foo{a:123}; Where we can use an Inference Variable
  // to handle the typing of the struct
  BaseType *unify (BaseType *other) override final
  {
    if (!base->can_resolve ())
      return BaseRules::unify (other);

    auto lookup = base->resolve ();
    return lookup->unify (other);
  }

  void visit (ParamType &type) override
  {
    if (base->get_symbol ().compare (type.get_symbol ()) != 0)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = type.clone ();
  }

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
  }

private:
  BaseType *get_base () override { return base; }

  ParamType *base;
};

class StrRules : public BaseRules
{
  // FIXME we will need a enum for the StrType like ByteBuf etc..
  using Rust::TyTy::BaseRules::visit;

public:
  StrRules (StrType *base) : BaseRules (base), base (base) {}

  void visit (StrType &type) override { resolved = type.clone (); }

private:
  BaseType *get_base () override { return base; }

  StrType *base;
};

class NeverRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  NeverRules (NeverType *base) : BaseRules (base), base (base) {}

  void visit (NeverType &type) override { resolved = type.clone (); }

private:
  BaseType *get_base () override { return base; }

  NeverType *base;
};

class PlaceholderRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  PlaceholderRules (PlaceholderType *base) : BaseRules (base), base (base) {}

  BaseType *unify (BaseType *other) override final
  {
    if (!base->can_resolve ())
      return BaseRules::unify (other);

    BaseType *lookup = base->resolve ();
    return lookup->unify (other);
  }

  void visit (PlaceholderType &type) override
  {
    if (base->get_symbol ().compare (type.get_symbol ()) != 0)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = type.clone ();
  }

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
  }

private:
  BaseType *get_base () override { return base; }

  PlaceholderType *base;
};

class DynamicRules : public BaseRules
{
  using Rust::TyTy::BaseRules::visit;

public:
  DynamicRules (DynamicObjectType *base) : BaseRules (base), base (base) {}

  void visit (InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
  }

  void visit (DynamicObjectType &type) override
  {
    if (base->num_specified_bounds () != type.num_specified_bounds ())
      {
	BaseRules::visit (type);
	return;
      }

    Location ref_locus = mappings->lookup_location (type.get_ref ());
    if (!base->bounds_compatible (type, ref_locus, true))
      {
	BaseRules::visit (type);
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

#endif // RUST_TYTY_RULES
