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

#ifndef RUST_TYTY_RULES
#define RUST_TYTY_RULES

#include "rust-diagnostics.h"
#include "rust-tyty.h"
#include "rust-tyty-visitor.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace TyTy {

class BaseRules : public TyVisitor
{
public:
  virtual ~BaseRules () {}

  BaseType *combine (BaseType *other)
  {
    other->accept_vis (*this);
    if (resolved != nullptr)
      {
	resolved->append_reference (base->get_ref ());
	resolved->append_reference (other->get_ref ());
	for (auto ref : base->get_combined_refs ())
	  resolved->append_reference (ref);
	for (auto ref : other->get_combined_refs ())
	  resolved->append_reference (ref);

	bool result_resolved = resolved->get_kind () != TyTy::TypeKind::INFER;
	if (result_resolved)
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
		    NodeId ref_node_id = UNKNOWN_NODEID;
		    context->insert_type (
		      Analysis::NodeMapping (mappings->get_current_crate (),
					     ref_node_id, ref,
					     UNKNOWN_LOCAL_DEFID),
		      resolved->clone ());
		  }
	      }
	  }
      }
    return resolved;
  }

  virtual void visit (UnitType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (TupleType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (ADTType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (InferType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (FnType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (ArrayType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (BoolType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (IntType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (UintType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (USizeType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (ISizeType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (FloatType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (ErrorType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (StructFieldType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (CharType &type) override
  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

  virtual void visit (ReferenceType &type) override

  {
    Location ref_locus = mappings->lookup_location (type.get_ref ());
    rust_error_at (ref_locus, "expected [%s] got [%s]",
		   base->as_string ().c_str (), type.as_string ().c_str ());
  }

protected:
  BaseRules (BaseType *base)
    : mappings (Analysis::Mappings::get ()),
      context (Resolver::TypeCheckContext::get ()), base (base),
      resolved (new ErrorType (base->get_ref (), base->get_ref ()))
  {}

  Analysis::Mappings *mappings;
  Resolver::TypeCheckContext *context;

  BaseType *base;
  BaseType *resolved;
};

class InferRules : public BaseRules
{
public:
  InferRules (InferType *base) : BaseRules (base), base (base) {}

  void visit (UnitType &type) override
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

private:
  InferType *base;
};

class StructFieldTypeRules : public BaseRules
{
public:
  StructFieldTypeRules (StructFieldType *base) : BaseRules (base), base (base)
  {}

  void visit (StructFieldType &type)
  {
    BaseType *ty = base->get_field_type ()->combine (type.get_field_type ());
    if (ty == nullptr)
      return;

    resolved = new TyTy::StructFieldType (type.get_ref (), type.get_ty_ref (),
					  type.get_name (), ty);
  }

private:
  StructFieldType *base;
};

class UnitRules : public BaseRules
{
public:
  UnitRules (UnitType *base) : BaseRules (base), base (base) {}

  void visit (UnitType &type) override
  {
    resolved = new UnitType (type.get_ref (), type.get_ty_ref ());
  }

private:
  UnitType *base;
};

class FnRules : public BaseRules
{
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

    // FIXME add an abstract method for is_equal on BaseType
    for (size_t i = 0; i < base->num_params (); i++)
      {
	auto a = base->param_at (i).second;
	auto b = type.param_at (i).second;

	auto combined_param = a->combine (b);
	if (combined_param == nullptr)
	  {
	    BaseRules::visit (type);
	    return;
	  }
      }

    auto combined_return
      = base->get_return_type ()->combine (type.get_return_type ());
    if (combined_return == nullptr)
      {
	BaseRules::visit (type);
	return;
      }

    resolved = base->clone ();
    resolved->set_ref (type.get_ref ());
  }

private:
  FnType *base;
};

class ArrayRules : public BaseRules
{
public:
  ArrayRules (ArrayType *base) : BaseRules (base), base (base) {}

  void visit (ArrayType &type) override
  {
    // check base type
    auto base_resolved = base->get_type ()->combine (type.get_type ());
    if (base_resolved == nullptr)
      {
	// fixme add error message
	return;
      }

    // need to check the base types and capacity
    if (type.get_capacity () != base->get_capacity ())
      {
	Location locus = mappings->lookup_location (type.get_ref ());
	rust_error_at (locus, "mismatch in array capacity");
	BaseRules::visit (type);
	return;
      }

    resolved = new ArrayType (type.get_ref (), type.get_ty_ref (),
			      type.get_capacity (), base_resolved);
  }

private:
  ArrayType *base;
};

class BoolRules : public BaseRules
{
public:
  BoolRules (BoolType *base) : BaseRules (base), base (base) {}

  void visit (BoolType &type) override
  {
    resolved = new BoolType (type.get_ref (), type.get_ty_ref ());
  }

private:
  BoolType *base;
};

class IntRules : public BaseRules
{
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
    if (type.get_kind () != base->get_kind ())
      {
	BaseRules::visit (type);
	return;
      }

    resolved
      = new IntType (type.get_ref (), type.get_ty_ref (), type.get_kind ());
  }

private:
  IntType *base;
};

class UintRules : public BaseRules
{
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
    if (type.get_kind () != base->get_kind ())
      {
	BaseRules::visit (type);
	return;
      }

    resolved
      = new UintType (type.get_ref (), type.get_ty_ref (), type.get_kind ());
  }

private:
  UintType *base;
};

class FloatRules : public BaseRules
{
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
    if (type.get_kind () != base->get_kind ())
      {
	BaseRules::visit (type);
	return;
      }

    resolved
      = new FloatType (type.get_ref (), type.get_ty_ref (), type.get_kind ());
  }

private:
  FloatType *base;
};

class ADTRules : public BaseRules
{
public:
  ADTRules (ADTType *base) : BaseRules (base), base (base) {}

  void visit (ADTType &type)
  {
    if (base->num_fields () != type.num_fields ())
      {
	BaseRules::visit (type);
	return;
      }

    std::vector<TyTy::StructFieldType *> fields;
    for (size_t i = 0; i < type.num_fields (); ++i)
      {
	TyTy::StructFieldType *base_field = base->get_field (i);
	TyTy::StructFieldType *other_field = type.get_field (i);

	BaseType *combined = base_field->combine (other_field);
	if (combined == nullptr)
	  {
	    BaseRules::visit (type);
	    return;
	  }

	fields.push_back ((TyTy::StructFieldType *) combined);
      }

    resolved = new TyTy::ADTType (type.get_ref (), type.get_ty_ref (),
				  type.get_name (), fields);
  }

private:
  ADTType *base;
};

class TupleRules : public BaseRules
{
public:
  TupleRules (TupleType *base) : BaseRules (base), base (base) {}

  void visit (TupleType &type)
  {
    if (base->num_fields () != type.num_fields ())
      {
	BaseRules::visit (type);
	return;
      }

    std::vector<HirId> fields;
    for (size_t i = 0; i < base->num_fields (); i++)
      {
	BaseType *bo = base->get_field (i);
	BaseType *fo = type.get_field (i);

	BaseType *combined = bo->combine (fo);
	if (combined == nullptr)
	  {
	    BaseRules::visit (type);
	    return;
	  }

	fields.push_back (combined->get_ref ());
      }

    resolved
      = new TyTy::TupleType (type.get_ref (), type.get_ty_ref (), fields);
  }

private:
  TupleType *base;
};

class USizeRules : public BaseRules
{
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
  USizeType *base;
};

class ISizeRules : public BaseRules
{
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
  ISizeType *base;
};

class CharRules : public BaseRules
{
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
  CharType *base;
};

class ReferenceRules : public BaseRules
{
public:
  ReferenceRules (ReferenceType *base) : BaseRules (base), base (base) {}

  void visit (ReferenceType &type) override
  {
    auto base_type = base->get_base ();
    auto other_base_type = type.get_base ();

    TyTy::BaseType *base_resolved = base_type->combine (other_base_type);
    resolved = new ReferenceType (base->get_ref (), base->get_ty_ref (),
				  base_resolved->get_ref ());
  }

private:
  ReferenceType *base;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_RULES
